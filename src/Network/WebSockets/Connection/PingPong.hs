{-# LANGUAGE LambdaCase #-}

module Network.WebSockets.Connection.PingPong
    ( withPingPong
    , PingPongOptions(..)
    , PongTimeout(..)
    , defaultPingPongOptions
    ) where

import Control.Concurrent.Async as Async
import Control.Concurrent.MVar (takeMVar)
import Control.Exception
import Control.Monad (void)
import qualified Data.ByteString.Lazy as BL
import Data.Function (fix)
import Network.WebSockets.Connection (Connection, connectionHeartbeat, pingThread)
import System.Timeout (timeout)


-- | Exception type used to kill connections if there
-- is a pong timeout.
data PongTimeout = PongTimeout (Maybe BL.ByteString) deriving Show

instance Exception PongTimeout


-- | Options for ping-pong
--
-- Make sure that the ping interval is less than the pong timeout,
-- for example N/2.
data PingPongOptions = PingPongOptions {
    pingInterval :: Int, -- ^ Interval in seconds
    pongTimeout :: Int, -- ^ Timeout in seconds
    pingAction :: Int -> IO () -- ^ Action to perform after sending a ping
}

-- | Default options for ping-pong
--
--   Ping every 15 seconds, timeout after 30 seconds
defaultPingPongOptions :: PingPongOptions
defaultPingPongOptions = PingPongOptions {
    pingInterval = 15,
    pongTimeout = 30,
    pingAction = const $ return ()
}

-- | Run an application with ping-pong enabled. Raises 'PongTimeout' if a pong
-- is not received.
--
-- Can used with Client and Server connections.
--
-- The implementation uses multiple threads, so if you want to call this from a
-- Monad other than 'IO', we recommend using
-- [unliftio](https://hackage.haskell.org/package/unliftio), e.g. using a
-- wrapper like this:
--
-- > withPingPongUnlifted
-- >     :: MonadUnliftIO m
-- >     => PingPongOptions -> Connection -> (Connection -> m ()) -> m ()
-- > withPingPongUnlifted options connection app = withRunInIO $ \run ->
-- >     withPingPong options connection (run . app)
withPingPong :: PingPongOptions -> Connection -> (Connection -> IO ()) -> IO ()
withPingPong options connection app = void $
    withAsync (app connection) $ \appAsync -> do
        withAsync (pingThread connection (pingInterval options) (pingAction options)) $ \pingAsync -> do
            withAsync (heartbeat >>= throwIO . PongTimeout) $ \heartbeatAsync -> do
                waitAnyCancel [appAsync, pingAsync, heartbeatAsync]
    where
        heartbeat :: IO (Maybe BL.ByteString)
        heartbeat = flip fix Nothing $ \loop lastResult ->
           timeout (pongTimeout options * 1000 * 1000) (takeMVar (connectionHeartbeat connection)) >>= \case
               Just result -> loop (Just result)
               Nothing -> return lastResult
