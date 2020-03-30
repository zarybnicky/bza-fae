{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Distributed.Process
  ( Process
  , ProcessId
  , expect
  , getSelfPid
  , nsend
  , register
  , say
  , send
  , spawnLocal
  , try
  , unregister
  , whereis
  )
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Backend.P2P
  ( bootstrap
  , getCapable
  , getPeers
  , makeNodeId
  , nsendCapable
  )
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Data.Binary (Binary)
import Data.ByteString.Char8 (pack)
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import RBC
import System.Environment (getArgs)
import System.Random.Dice (getRandomRs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    host:port:seeds ->
      bootstrap host port (host,) initRemoteTable (makeNodeId <$> seeds) mainProcess
    _ -> putStrLn "Usage: hbbft-cloud host port [seeds...]"

data Rbc
  = Local String
  | RemoteInit String [ProcessId]
  | RemoteIn String ProcessId In
  deriving (Generic, Binary, Typeable)

mainProcess :: Process ()
mainProcess = do
  mgr <- spawnLocal rbcManager
  forever $
    words <$> liftIO getLine >>= \case
      ["peers"] -> getPeers >>= say . show
      ["in", r] -> getCapable r >>= say . show
      "start":msg -> send mgr (Local $ unwords msg)
      _ -> say "peers | in <r> | tell <msg>"

rbcManager :: Process ()
rbcManager = do
  mgr <- getSelfPid
  register "rbcManager" mgr
  forever $ (expect :: Process Rbc) >>= \case
    RemoteIn n src i -> nsend n (src, i)
    RemoteInit n validators -> void $ spawnLocal (rbcInstance n mgr validators)
    Local rbcMessage -> do
      say rbcMessage
      rbcName <- liftIO $ show . head <$> getRandomRs (1, 1000000) 1
      validators <- sort <$> getCapable "rbcManager"
      forM_ (filter (/= mgr) validators) (`send` RemoteInit rbcName validators)
      pid <- spawnLocal (rbcInstance rbcName mgr validators)
      send pid (mgr, Input (pack rbcMessage))

rbcInstance :: String -> ProcessId -> [ProcessId] -> Process ()
rbcInstance name self validators = do
  getSelfPid >>= register name
  say $ "Starting RBC " <> name <> " with validators " <> show validators
  case elemIndex self validators of
    Nothing -> pure ()
    Just idx -> go (setup (length validators) idx)
      where
        go s = do
          (src, msg) <- expect :: Process (ProcessId, In)
          say ("Received " <> show msg)
          let (out, s') = runState (receive msg (fromJust $ elemIndex src validators)) s
          result <- out
          case result of
            Output msg -> do
              say $ "Decoded: " <> show msg
              unregister name
            Broadcast msgs -> do
              forM_ msgs $ \(msg, nid) -> send (validators !! nid) (RemoteIn name self msg)
              go s'
            x -> say (show x) >> go s'
