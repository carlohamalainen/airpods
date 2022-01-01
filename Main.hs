{-# OPTIONS -Wall                   #-}
{-# OPTIONS -Wno-name-shadowing     #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Prelude
import Data.Foldable
import Control.Monad
import Data.Char (digitToInt)
import Text.Parsec

import Control.Concurrent (threadDelay)

import qualified Shh as S
import           Shh ((|>))

import System.Console.GetOpt
import System.Environment

data SinkLine
    = Mac String
    | Other String
  deriving (Eq, Show)

data Device = Device Int [SinkLine]
  deriving Show

type Parse t = forall s m. Stream s m Char => ParsecT s () m t

number :: Parse Int
number = number' <$> many1 digit
  where
    number' :: Integral i => String -> i
    number' = foldl (\x -> ((10*x) +) . fromIntegral . digitToInt) 0

pSinksAvailable :: Parse Int
pSinksAvailable = do
    nrSinks <- number
    void $ string " sink(s) available."
    void endOfLine

    return nrSinks

pSinkInputsAvailable :: Parse Int
pSinkInputsAvailable = do
    nrSinks <- number
    void $ string " sink input(s) available."
    void endOfLine

    return nrSinks

pIndex :: Parse Int
pIndex = do
    void $ manyTill anyChar (try $ string "index: ")
    n <- number
    void endOfLine

    return n

pOtherLine :: Parse SinkLine
pOtherLine = do
    void $ many1 tab
    xs <- manyTill anyChar $ lookAhead endOfLine
    void endOfLine

    return $ Other xs

pDeviceLine :: Parse SinkLine
pDeviceLine = do
    void $ many1 tab
    void $ string "device.string = \""
    mac <- manyTill anyChar $ lookAhead (string "\"")
    void $ string "\""
    void endOfLine

    return $ Mac mac

pDevice :: Parse Device
pDevice = Device <$> pIndex <*> many1 (try pDeviceLine <|> try pOtherLine)

pSinkInputs :: Parse [Int]
pSinkInputs = do
    n <- pSinkInputsAvailable
    replicateM n $ do
        i <- pIndex
        void $ many1 tab
        void $ manyTill anyChar $ lookAhead endOfLine
        void endOfLine
        return i

pDevices :: Parse [Device]
pDevices = do
    n <- pSinksAvailable
    replicateM n pDevice

moveInputs :: Int -> [Int] -> IO ()
moveInputs airpodIndex sinks
    = forM_ sinks $ \s -> do
        print ("move-sink-input", s, "==>", airpodIndex)
        S.exe "pacmd" "move-sink-input" (show s) (show airpodIndex)

disconnect :: String -> IO ()
disconnect mac = do
    x <- S.exitCode $ S.exe "bluetoothctl" "disconnect" mac
    unless (x == 0) $ error $
        "Disconnect failed: " <> show x

connect :: String -> IO ()
connect mac = do
    x <- S.exitCode $ S.exe "bluetoothctl" "connect" mac
    unless (x == 0) $ error $
        "Connect failed: " <> show x

pLine :: Parse (Maybe String)
pLine = do
    void $ manyTill anyChar $ lookAhead endOfLine
    void endOfLine
    return Nothing

pHciDev :: String -> Parse (Maybe String)
pHciDev mac = asum <$> many1 (try pHciDev' <|> pLine)
  where
    pHciDev' :: Parse (Maybe String)
    pHciDev' = do
        void $ many1 space

        void $ string "object path \"/org/bluez/hci"
        n  <- number
        void $ string "/dev_"

        mac' <- string mac

        void $ string "/"
        sep <- manyTill anyChar (try $ char '/')
        void $ string "fd"

        n' <- number

        void $ string "\""

        return $ Just $
            "/org/bluez/hci" <> show n <> "/dev_" <> mac' 
            <> "/" <> sep <> "/fd" <> show n'

-- Set the internal volume of the AirPods. This requires a patched bluez!
-- 
-- https://carlo-hamalainen.net/2021/05/20/airpods
--
setAirPodsVolume :: String -> Int -> IO ()
setAirPodsVolume device volume = S.exe
    "dbus-send"
    "--print-reply"
    "--system"
    "--dest=org.bluez"
    device
    "org.freedesktop.DBus.Properties.Set"
    "string:org.bluez.MediaTransport1"
    "string:Volume"
    ("variant:uint16:" <> show volume)

setVolume :: Int -> IO ()
setVolume airpodIndex = S.exe "pacmd" "set-sink-volume" (show airpodIndex) "20000"

setDefaultSink :: Int -> IO ()
setDefaultSink airpodIndex = S.exe "pacmd" "set-default-sink" (show airpodIndex)

getInputs :: IO [Int]
getInputs = do
    inputs <- S.exe "pacmd" "list-sink-inputs" |> S.capture

    inputs <- runParserT pSinkInputs () "sink_inputs" inputs

    return $ case inputs of
        Left  _   -> []
        Right ixs -> ixs

getAirpod :: (S.Shell m, MonadFail m) => [Char] -> m Int
getAirpod mac = do
    x <- S.exe "pacmd" "list-sinks" |> S.capture

    x <- runParserT pDevices () "foo" x

    case x of
        Left err -> error $ "Failed to parse an AirPod: " <> show err
        Right x' -> return $ case asum $ map f x' of
            Nothing -> error "No AirPods found"
            Just i  -> i

  where

    f (Device i slines)
        = case filter matchMac slines of
            [] -> Nothing
            _  -> Just i

    matchMac = \case Mac m -> m == mac
                     _     -> False

getHciDev :: (S.Shell m, MonadFail m) => String -> m String
getHciDev mac_ = do
    x <- S.exe
            "dbus-send" 
            "--print-reply"
            "--system" 
            "--dest=org.bluez"
            "/"
            "org.freedesktop.DBus.ObjectManager.GetManagedObjects"
        |> S.capture 
   
    h <- runParserT (pHciDev mac_) () "hci" x

    case h of
        Left err -> error $ show err
        Right Nothing -> error "empty parse in getHciDev"
        Right (Just h') -> return h'

newtype Flag = MacAddress String
  deriving Show

options :: [OptDescr Flag]
options = [ Option ['m'] ["mac"] (ReqArg MacAddress "MAC ADDRESS") "MAC address of your AirPods" ]

airpodOpts :: [String] -> IO [Flag]
airpodOpts argv =
  case getOpt Permute options argv of
     (o, _n, []  ) -> return o
     (_, _,  errs) -> ioError (userError (concat errs ++ usageInfo header options))

header :: String
header = "Usage: airpods <OPTION>"

main :: IO ()
main = do
    argv <- getArgs
    opts <- airpodOpts argv

    mac <- case opts of
            (MacAddress m:_) -> return m
            _ -> ioError $ userError $ usageInfo header options

    putStrLn $ "::: Disconnecting " <> mac
    disconnect mac

    putStrLn $ "::: Connecting " <> mac
    connect    mac

    threadDelay $ 3 * 1000000

    inputs <- getInputs

    airpodIndex <- getAirpod mac

    putStrLn $ "::: Moving inputs for " <> mac <> " at index " <> show airpodIndex
    moveInputs airpodIndex inputs

    let f ':' = '_'
        f  c  = c

    h <- getHciDev $ map f mac

    putStrLn "::: Setting AirPods internal volume"
    setAirPodsVolume h 90 

    putStrLn "::: Setting AirPods main volume"
    setVolume airpodIndex

    putStrLn "::: Setting AirPods as default sink"
    setDefaultSink airpodIndex
