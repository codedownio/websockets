{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.WebSockets as WS
import UnliftIO.Exception


wsHandler :: WS.PendingConnection -> IO ()
wsHandler pending = do
  conn <- WS.acceptRequest pending
  putStrLn "Client connected to server"

  WS.sendTextData conn ("hello world" :: T.Text)
  putStrLn "Server sent: hello world"

  let pingPongOpts = WS.defaultPingPongOptions {
        WS.pingInterval = 30
        , WS.pongTimeout = 60
        , WS.pingAction = putStrLn "Server sent ping"
        }
  WS.withPingPong pingPongOpts conn $ \_ -> do
    putStrLn "Server entering idle state with ping/pong enabled"
    forever $ do
      threadDelay 1000000  -- 1 second

wsClient :: IO ()
wsClient = do
  putStrLn "Client starting..."
  threadDelay 500_000

  WS.runClient "127.0.0.1" 8080 "/" $ \conn -> do
    putStrLn "Client connected to server"

    withException (clientLoop conn) $ \e -> do
      putStrLn $ "Client exited with exception: " ++ displayException (e :: SomeException)

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
  putStrLn "Starting ping-pong test..."
  putStrLn "Server will send pings, client will ignore them"
  putStrLn "Press Ctrl+C to stop"

  clientThread <- async wsClient

  let serverOpts = WS.defaultServerOptions
          { WS.serverHost = "127.0.0.1"
          , WS.serverPort = 8080
          , WS.serverConnectionOptions = WS.defaultConnectionOptions
              { WS.connectionTimeout = 3600  -- seconds
              }
          }

  putStrLn "Starting server on 127.0.0.1:8080..."
  _ <- WS.runServerWithOptions serverOpts wsHandler

  wait clientThread
