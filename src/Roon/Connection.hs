-- |
-- Module      : Roon.Connection
-- Description : Connecting to a Roon Server as an extension
-- License     : BSD-3-Clause
module Roon.Connection
  ( withRoon
  , runRoon
  , registerExtension
  ) where

import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Roon.Effect.Moo (Moo, defaultRequestHandler, pingService, runMoo)
import Roon.Effect.TokenStore (TokenStore, loadToken, saveToken)
import Roon.Effect.WebSocket (ServerAddress, WebSocket, runWebSocket)
import Roon.Error (RoonError)
import Roon.Registry (CoreInfo (..), ExtensionInfo (..), Registration (..), info, register)

-- | Register the extension, reusing the stored token if there is one, and
-- store the token that the server returns.
--
-- The first time, this waits until the user enables the extension in
-- Roon, under Settings, Extensions.
registerExtension
  :: (Moo :> es, TokenStore :> es, Error RoonError :> es)
  => ExtensionInfo
  -> Eff es Registration
registerExtension extension = do
  core <- info
  token <- loadToken core.coreId
  registration <- register extension token
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
runRoon extension action =
  runMoo defaultRequestHandler (registerExtension withPing >>= action)
  where
    withPing
      | pingService `elem` extension.providedServices = extension
      | otherwise = extension {providedServices = pingService : extension.providedServices}

-- | Connect to a Roon Server, register the extension and run an action.
-- The connection is closed when the action returns.
withRoon
  :: (IOE :> es, Concurrent :> es, TokenStore :> es, Error RoonError :> es)
  => ServerAddress
  -> ExtensionInfo
  -> (Registration -> Eff (Moo : WebSocket : es) a)
  -> Eff es a
withRoon address extension = runWebSocket address . runRoon extension
