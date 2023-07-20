{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Main (main) where

{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}

import Data.Kind

import Prelude
import Data.Foldable
import Control.Monad

import Data.Text qualified as T
import Data.List qualified as List

import Control.Lens
import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.KeyMap qualified as KeyMap

import Control.Concurrent (threadDelay)

import Shh qualified as S
import           Shh ((|>))

import System.Console.GetOpt
import System.Environment
import Data.Aeson.Lens

moveInputs :: Integer -> [Integer] -> IO ()
moveInputs airpodIndex sinks
    = forM_ sinks $ \s -> do
        print ("move-sink-input", s, "==>", airpodIndex)
        S.exe "pactl" "move-sink-input" (show s) (show airpodIndex)

restartBluetooth :: IO ()
restartBluetooth = do
    x <- S.exitCode $ S.exe "sudo" "systemctl" "disable" "bluetooth"
    unless (x == 0) $ error $
        "enable bluetooth failed: " <> show x

    x <- S.exitCode $ S.exe "sudo" "systemctl" "enable" "bluetooth"
    unless (x == 0) $ error $
        "enable bluetooth failed: " <> show x

    x <- S.exitCode $ S.exe "sudo" "systemctl" "restart" "bluetooth"
    unless (x == 0) $ error $
        "restart bluetooth failed: " <> show x

disconnect :: String -> IO ()
disconnect mac = do
    x <- S.exitCode $ S.exe "bluetoothctl" "disconnect" mac
    unless (x == 0) $ error $
        "Disconnect failed: " <> show x

trust :: String -> IO ()
trust mac = do
    x <- S.exitCode $ S.exe "bluetoothctl" "trust" mac
    unless (x == 0) $ error $
        "Connect failed: " <> show x

connect :: String -> IO ()
connect mac = do
    x <- S.exitCode $ S.exe "bluetoothctl" "connect" mac
    unless (x == 0) $ error $
        "Connect failed: " <> show x

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

setVolume :: Integer -> IO ()
setVolume airpodIndex = S.exe "pactl" "set-sink-volume" (show airpodIndex) "20000" -- 20%

setDefaultSink :: Integer -> IO ()
setDefaultSink airpodIndex = S.exe "pactl" "set-default-sink" (show airpodIndex)

getInputs :: IO [Integer]
getInputs = do
    inputs <- S.exe "pactl" "-f" "json" "list" "sink-inputs" Shh.|> S.capture

    case eitherDecode inputs of
        Left err -> error err
        Right (j::Value) -> do
            return $ j ^.. _Array . each . key (fromString "index") . _Integer

getAirpod :: (S.Shell m, Monad m) => String -> m (Either String Integer)
getAirpod mac = do
    x <- S.exe "pactl" "-f" "json" "list" "sinks" Shh.|> S.capture

    case eitherDecode x of
        Left err -> return $ Left err
        Right (j::Value) -> do

            let xs :: [Value]
                xs = j ^.. _Array . each

                fff :: Value -> Bool
                fff v = Just (T.pack mac) == v ^? key (fromString "properties") . key (fromString "device.string") . _String

                xs' :: [Value]
                xs' = xs ^.. each . filtered fff

                air :: Maybe Integer
                air = xs' ^? each . key (fromString "index") . _Integer

            return $ case air of
                        Nothing -> Left $ "No match for " <> mac
                        Just x  -> Right x

getHciDev :: (S.Shell m, MonadFail m) => String -> m String
getHciDev mac = do
    x <- S.exe
            "busctl"
            "call"
            "org.bluez"
            "/"
            "org.freedesktop.DBus.ObjectManager"
            "GetManagedObjects"
            "--json"
            "pretty"
        Shh.|> S.capture

    let isFD s = "fd" == s ^. reversed . to (take 3) . to (drop 1) . reversed

        isMAC s = map f mac `List.isInfixOf` s
          where
            f ':' = '_'
            f  c  = c

    case eitherDecode x of
        Left err -> error err
        Right (j::Value) -> do
            let j' = j ^? key (fromString "data")
                   . _Array . each . _Object
                   . to (KeyMap.filterWithKey (\k _v -> let k' = toString k in isFD k' && isMAC k'))

                h = j' ^? _Just . to KeyMap.toList . _head . _1 . to toString

            case h of
                Nothing -> error $ "Could not find HCI device info in " <> show j
                Just h' -> return h'

type Flag :: Type

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

retryM :: [Int] -> IO (Either a b) -> IO b
retryM [] _ = error "too many retries"
retryM (s:ss) f = do
    x <- f

    case x of
        Right x' -> return x'
        Left _ -> do
            threadDelay $ s * 1000000
            retryM ss f

main :: IO ()
main = do
    argv <- getArgs
    opts <- airpodOpts argv

    mac <- case opts of
            (MacAddress m:_) -> return m
            _ -> ioError $ userError $ usageInfo header options

    restartBluetooth
    threadDelay $ 3 * 1000000

    putStrLn $ "::: Disconnecting " <> mac
    disconnect mac

    putStrLn $ "::: Trusting " <> mac
    trust mac

    putStrLn $ "::: Connecting " <> mac
    connect mac

    threadDelay $ 3 * 1000000


    airpodIndex <- retryM [1, 2, 3, 5, 7] (getAirpod mac)

    putStrLn $ "Found Airpod with index " <> show airpodIndex

    inputs <- getInputs

    print inputs

    putStrLn $ "::: Moving inputs for " <> mac <> " at index " <> show airpodIndex
    moveInputs airpodIndex inputs

    h <- getHciDev mac

    putStrLn "::: Setting AirPods internal volume"
    setAirPodsVolume h 90

    putStrLn "::: Setting AirPods main volume"
    setVolume airpodIndex

    putStrLn "::: Setting AirPods as default sink"
    setDefaultSink airpodIndex
