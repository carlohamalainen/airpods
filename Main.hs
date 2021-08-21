{-# OPTIONS -Wall                   #-}
{-# OPTIONS -Wno-name-shadowing     #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}

module Main (main) where

import Prelude
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

number :: Stream s m Char => ParsecT s u m Int
number = number' <$> many1 digit
  where
    number' :: Integral i => String -> i
    number' = foldl (\x -> ((10*x) +) . fromIntegral . digitToInt) 0

pSinksAvailable :: Stream s m Char => ParsecT s u m Int
pSinksAvailable = do
    nrSinks <- number
    void $ string " sink(s) available."
    void endOfLine

    return nrSinks

pSinkInputsAvailable :: Stream s m Char => ParsecT s u m Int
pSinkInputsAvailable = do
    nrSinks <- number
    void $ string " sink input(s) available."
    void endOfLine

    return nrSinks

pIndex :: Stream s m Char => ParsecT s u m Int
pIndex = do
    void $ manyTill anyChar (try $ string "index: ")
    n <- number
    void endOfLine

    return n

pOtherLine :: Stream s m Char => ParsecT s u m SinkLine
pOtherLine = do
    void $ many1 tab
    xs <- manyTill anyChar $ lookAhead endOfLine
    void endOfLine

    return $ Other xs

pDeviceLine :: Stream s m Char => ParsecT s u m SinkLine
pDeviceLine = do
    void $ many1 tab
    void $ string "device.string = \""
    mac <- manyTill anyChar $ lookAhead (string "\"")
    void $ string "\""
    void endOfLine

    return $ Mac mac

pDevice :: Stream s m Char => ParsecT s u m Device
pDevice = do
    i  <- pIndex
    xs <- many1 (try pDeviceLine <|> try pOtherLine)

    return $ Device i xs

pSinkInputs :: Stream s m Char => ParsecT s u m [Int]
pSinkInputs = do
    n <- pSinkInputsAvailable
    replicateM n $ do
        i <- pIndex
        void $ many1 tab
        void $ manyTill anyChar $ lookAhead endOfLine
        void endOfLine
        return i

pDevices :: Stream s m Char => ParsecT s u m [Device]
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

pLine :: Stream s m Char => ParsecT s u m (Maybe String)
pLine = do
    void $ manyTill anyChar $ lookAhead endOfLine
    void endOfLine
    return Nothing

pHciDev :: Stream s m Char => String -> ParsecT s u m (Maybe String)
pHciDev mac = foldl mplus Nothing
           <$> many1 (try pHciDev' <|> pLine)
  where
    pHciDev' :: Stream s m Char => ParsecT s u m (Maybe String)
    pHciDev' = do
        void $ many1 space

        void $ string "object path \"/org/bluez/hci"
        n  <- number
        void $ string "/dev_"

        mac' <- string mac

        void $ string "/fd"

        n' <- number

        void $ string "\""

        return $ Just $
            "/org/bluez/hci" <> show n <> "/dev_" <> mac' <> "/fd" <> show n'

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
        Right x' -> return $ case foldl mplus Nothing $ map f x' of
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
   
    Right (Just h) <- runParserT (pHciDev mac_) () "hci" x

    return h

newtype Flag = MacAddress String
  deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['m'] ["mac"] (ReqArg MacAddress "MAC ADDRESS") "MAC address of your AirPods"
    ]

compilerOpts :: [String] -> IO [Flag]
compilerOpts argv =
  case getOpt Permute options argv of
     (o, _n, []  ) -> return o
     (_, _,  errs) -> ioError (userError (concat errs ++ usageInfo header options))

header :: String
header = "Usage: airpods <OPTION>"

main :: IO ()
main = do
    argv <- getArgs
    opts <- compilerOpts argv

    mac <- case opts of
            (MacAddress m:_) -> return m
            _ -> ioError $ userError $ usageInfo header options

    disconnect mac
    connect    mac

    threadDelay $ 3 * 1000000

    inputs <- getInputs

    airpodIndex <- getAirpod mac

    moveInputs airpodIndex inputs

    let f ':' = '_'
        f  c  = c

    h <- getHciDev $ map f mac

    setAirPodsVolume h 90 

    setVolume airpodIndex
