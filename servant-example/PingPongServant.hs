{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import qualified Network.WebSockets as WS
import Servant
import Servant.API.WebSocket (WebSocket)
import UnliftIO.Exception


type API = "ws" :> WebSocket

api :: Proxy API
api = Proxy

wsHandler :: forall m. MonadIO m => WS.Connection -> m ()
wsHandler conn = liftIO $ flip withException handler $ do
  putStrLn "Client connected to server"

  WS.sendTextData conn ("hello world" :: T.Text)
  putStrLn "Server sent: hello world"

  let pingPongOpts = WS.defaultPingPongOptions {
        WS.pingAction = putStrLn "Server sent ping"
        }
  WS.withPingPong pingPongOpts conn $ \_ -> do
    putStrLn "Server entering idle state with ping/pong enabled"
    forever $ do
      threadDelay 1000000  -- 1 second
  where
    handler :: SomeException -> IO ()
    handler e = putStrLn ("Server got exception: " <> show e)

server :: Server API
server = wsHandler

app :: Application
app = serve api server

wsClient :: IO ()
wsClient = flip withException handler $ do
  putStrLn "Client starting..."
  threadDelay 500_000

  WS.runClient "127.0.0.1" 8080 "/ws" $ \conn -> do
    putStrLn "Client connected to server"
    clientLoop conn

  where
    handler :: SomeException -> IO ()
    handler e = do
      putStrLn $ "Client exited with exception: " ++ displayException e

clientLoop :: WS.Connection -> IO ()
clientLoop conn = forever $ do
  WS.receive conn >>= \case
    WS.ControlMessage (WS.Ping _) -> do
      putStrLn "Client received ping (ignoring, not sending pong)"
    WS.ControlMessage (WS.Pong _) -> do
      putStrLn "Client received pong"
    WS.ControlMessage (WS.Close _ _) -> do
      putStrLn "Client received close"
    WS.DataMessage _ _ _ payload -> do
      let text = case payload of
              WS.Text bs _ -> T.decodeUtf8 (BL.toStrict bs)
              WS.Binary bs -> T.decodeUtf8 (BL.toStrict bs)
      putStrLn $ "Client received data: " ++ T.unpack text

main :: IO ()
main = do
  putStrLn "Starting ping-pong test (Warp + Servant + servant-websockets version)..."
  putStrLn "Server will send pings, client will ignore them"
  putStrLn "Press Ctrl+C to stop"

  clientThread <- async wsClient

  putStrLn "Starting server on 127.0.0.1:8080..."
  let warpSettings = setPort 8080
                   $ setHost "127.0.0.1"
                   $ setTimeout 3600
                   $ defaultSettings
  _ <- async $ runSettings warpSettings app

  wait clientThread
