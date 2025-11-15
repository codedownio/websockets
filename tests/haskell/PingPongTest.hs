{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Exception (SomeException, catch, displayException)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.WebSockets as WS

-- | The websocket server handler
wsHandler :: WS.PendingConnection -> IO ()
wsHandler pending = do
  conn <- WS.acceptRequest pending
  putStrLn "Client connected to server"

  -- Send hello world message
  WS.sendTextData conn ("hello world" :: T.Text)
  putStrLn "Server sent: hello world"

  -- Enter idle state with ping/pong support
  -- withPingPong will automatically handle sending pings every 30 seconds
  WS.withPingPong conn 30 (putStrLn "Server sent ping") $ do
    putStrLn "Server entering idle state with ping/pong enabled"
    -- Just wait forever
    forever $ do
      threadDelay 1000000  -- 1 second

-- | The websocket client
wsClient :: IO ()
wsClient = do
  putStrLn "Client starting..."
  threadDelay 500000  -- Wait 500ms for server to start

  WS.runClient "127.0.0.1" 8080 "/" $ \conn -> do
    putStrLn "Client connected to server"

    -- Wrap the receive loop in catch to print the final exception
    catch (clientLoop conn) $ \e -> do
      putStrLn $ "Client exited with exception: " ++ displayException (e :: SomeException)

clientLoop :: WS.Connection -> IO ()
clientLoop conn = forever $ do
  -- Use receive instead of receiveDataMessage
  msg <- WS.receive conn
  case msg of
      WS.ControlMessage (WS.Ping _) -> do
        -- Ignore ping messages (don't send pong)
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

  -- Start the client in a separate thread
  clientThread <- async wsClient

  -- Start the server with custom connection options
  -- We set a large connection timeout (3600 seconds)
  let serverOpts = WS.defaultConnectionOptions
          { WS.connectionTimeout = Just 3600000000  -- 3600 seconds in microseconds
          }

  putStrLn "Starting server on 127.0.0.1:8080..."
  WS.runServerWith "127.0.0.1" 8080 serverOpts wsHandler

  -- Wait for client to finish (it won't, unless there's an exception)
  wait clientThread
