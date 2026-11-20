-- |
-- Module      : Roon.Connection
-- Description : Connecting to a Roon Server as an extension
-- License     : BSD-3-Clause
module Roon.Connection
  ( withRoon
  , withRoonWithin
  , runRoon
  , runRoonWithin
  , registerExtension
  , registerExtensionWithin

    -- * Reconnection
  , Reconnect (..)
  , defaultReconnect
  , reconnecting
  , withRoonReconnecting
  ) where

import Effectful
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar)
import Effectful.Error.Static (Error, throwError, tryError)
import Roon.Effect.Moo (Moo, defaultRequestHandler, pingService, runMoo)
import Roon.Effect.TokenStore (TokenStore, loadToken, saveToken)
import Roon.Effect.WebSocket (ServerAddress, WebSocket, runWebSocket)
import Roon.Error (RoonError (..))
import Roon.Registry (CoreInfo (..), ExtensionInfo (..), Registration (..), info, register, registerWithin)

-- | Register the extension, reusing the stored token if there is one, and
-- store the token that the server returns.
--
-- The first time, this waits until the user enables the extension in
-- Roon, under Settings, Extensions.
registerExtension
  :: (Moo :> es, TokenStore :> es, Error RoonError :> es)
  => ExtensionInfo
  -> Eff es Registration
registerExtension = registerExtensionWithin Nothing

-- | 'registerExtension', waiting at most a number of microseconds for the
-- server to answer the registration, and so for the user to enable the
-- extension; 'Nothing' waits as long as needed. Throws 'TimedOut' when the
-- time is up.
registerExtensionWithin
  :: (Moo :> es, TokenStore :> es, Error RoonError :> es)
  => Maybe Int
  -> ExtensionInfo
  -> Eff es Registration
registerExtensionWithin limit extension = do
  core <- info
  token <- loadToken core.coreId
  registration <- maybe register registerWithin limit extension token
  saveToken registration.core.coreId registration.token
  pure registration

-- | Register the extension over an established transport and run an
-- action. 'withRoon' is this over a real WebSocket.
--
-- The ping service, which every extension must provide, is added to the
-- provided services and answered automatically.
runRoon
  :: (WebSocket :> es, Concurrent :> es, TokenStore :> es, Error RoonError :> es)
  => ExtensionInfo
  -> (Registration -> Eff (Moo : es) a)
  -> Eff es a
runRoon = runRoonWithin Nothing

-- | 'runRoon', with a limit on the registration ('registerExtensionWithin').
runRoonWithin
  :: (WebSocket :> es, Concurrent :> es, TokenStore :> es, Error RoonError :> es)
  => Maybe Int
  -> ExtensionInfo
  -> (Registration -> Eff (Moo : es) a)
  -> Eff es a
runRoonWithin limit extension action =
  runMoo defaultRequestHandler (registerExtensionWithin limit withPing >>= action)
  where
    withPing
      | pingService `elem` extension.providedServices = extension
      | otherwise = extension {providedServices = pingService : extension.providedServices}

-- | Connect to a Roon Server, register the extension and run an action.
--
-- Roon keeps one connection per extension. Observed with Roon Server 2.75:
-- when a second connection of the same extension registers, Roon sends an
-- empty frame to the first one, which then fails with a 'ProtocolViolation'.
-- A program that connects for each task should take its connections one at
-- a time.
-- The connection is closed when the action returns.
withRoon
  :: (IOE :> es, Concurrent :> es, TokenStore :> es, Error RoonError :> es)
  => ServerAddress
  -> ExtensionInfo
  -> (Registration -> Eff (Moo : WebSocket : es) a)
  -> Eff es a
withRoon = withRoonWithin Nothing

-- | 'withRoon', with a limit on the registration
-- ('registerExtensionWithin'): for a program that cannot wait for the user
-- to enable the extension.
withRoonWithin
  :: (IOE :> es, Concurrent :> es, TokenStore :> es, Error RoonError :> es)
  => Maybe Int
  -> ServerAddress
  -> ExtensionInfo
  -> (Registration -> Eff (Moo : WebSocket : es) a)
  -> Eff es a
withRoonWithin limit address extension = runWebSocket address . runRoonWithin limit extension

-- | When to try again after the connection is lost or refused.
data Reconnect = Reconnect
  { firstDelay :: Int
  -- ^ Microseconds before the first new attempt. The delay doubles after
  -- each failed attempt, and starts again from this one once an attempt
  -- has connected.
  , maxDelay :: Int
  -- ^ Microseconds the delay never exceeds.
  , maxAttempts :: Maybe Int
  -- ^ How many attempts in a row may fail; 'Nothing' for no limit.
  }
  deriving stock (Eq, Show)

-- | One second, doubling up to one minute, without end.
defaultReconnect :: Reconnect
defaultReconnect = Reconnect {firstDelay = 1_000_000, maxDelay = 60_000_000, maxAttempts = Nothing}

-- | Run an action again whenever it fails with 'ConnectionLost' or
-- 'ConnectFailed', after a growing delay. The action receives a function
-- to call once it is connected, which resets the delay and the count of
-- failed attempts. Other failures, and the last one when 'maxAttempts' is
-- reached, are thrown.
reconnecting
  :: (Concurrent :> es, Error RoonError :> es)
  => Reconnect
  -> (Eff es () -> Eff es a)
  -> Eff es a
reconnecting policy attempt = go policy.firstDelay (1 :: Int)
  where
    go delay count = do
      connected <- newTVarIO False
      outcome <- tryError @RoonError (attempt (atomically (writeTVar connected True)))
      wasConnected <- atomically (readTVar connected)
      let (delay', count') = if wasConnected then (policy.firstDelay, 1) else (delay, count)
      case outcome of
        Right value -> pure value
        Left (_, failure)
          | retriable failure && maybe True (count' <) policy.maxAttempts -> do
              threadDelay delay'
              go (min policy.maxDelay (2 * delay')) (count' + 1)
          | otherwise -> throwError failure
    retriable = \case
      ConnectionLost -> True
      ConnectFailed _ -> True
      _ -> False

-- | 'withRoon', connecting again when the connection is lost or refused.
-- The action runs again from the start on each new connection, so the
-- subscriptions it opens are opened again.
withRoonReconnecting
  :: (IOE :> es, Concurrent :> es, TokenStore :> es, Error RoonError :> es)
  => Reconnect
  -> ServerAddress
  -> ExtensionInfo
  -> (Registration -> Eff (Moo : WebSocket : es) a)
  -> Eff es a
withRoonReconnecting policy address extension action =
  reconnecting policy $ \markConnected ->
    withRoon address extension $ \registration -> do
      raise (raise markConnected)
      action registration
