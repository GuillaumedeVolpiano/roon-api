-- The FileSystem constraint of openBinaryTempFile is what grants the
-- access; GHC cannot see it used, as with the lifted functions of
-- Effectful.FileSystem.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module      : Roon.Effect.TokenStore
-- Description : Where the authorisation tokens are kept
-- License     : BSD-3-Clause
--
-- When the user enables an extension in Roon, the server hands it a
-- token. Presenting that token on later connections skips the manual
-- step. There is one token per server, identified by its core id.
module Roon.Effect.TokenStore
  ( -- * Effect
    TokenStore (..)
  , loadToken
  , saveToken

    -- * Interpreters
  , runTokenStoreFile
  , defaultTokenFile
  , runTokenStorePure
  ) where

import Data.Aeson (decodeStrict, encode, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict (Map, empty, insert)
import Data.Map.Strict qualified as M (lookup)
import Data.Text (Text, unpack)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, type (:>))
import Effectful.Dispatch.Dynamic (interpret_, reinterpret_, send)
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Exception (bracketOnError)
import Effectful.FileSystem (FileSystem, XdgDirectory (XdgState),
                             createDirectoryIfMissing, doesFileExist,
                             getXdgDirectory, removeFile, renameFile)
import Effectful.FileSystem.IO (Handle, hClose)
import Effectful.FileSystem.IO.ByteString qualified as FS (readFile)
import Effectful.FileSystem.IO.ByteString.Lazy (hPut)
import Effectful.State.Static.Local (gets, modify, runState)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO qualified as IO (openBinaryTempFile)

-- | Remember the token that each server gave to this extension.
data TokenStore :: Effect where
  LoadToken :: Text -> TokenStore m (Maybe Text)
  SaveToken :: Text -> Text -> TokenStore m ()

type instance DispatchOf TokenStore = Dynamic

-- | The token for the server with this core id, if one was saved.
loadToken :: TokenStore :> es => Text -> Eff es (Maybe Text)
loadToken = send . LoadToken

-- | Save the token for the server with this core id.
saveToken :: TokenStore :> es => Text -> Text -> Eff es ()
saveToken coreId token = send (SaveToken coreId token)

-- | Keep the tokens in a JSON file.
--
-- The file is created readable by its owner only, and replaced
-- atomically. A missing or unreadable file counts as empty: the cost is
-- to enable the extension in Roon once more.
runTokenStoreFile :: FileSystem :> es => FilePath -> Eff (TokenStore : es) a -> Eff es a
runTokenStoreFile path = interpret_ $ \case
  LoadToken coreId -> M.lookup coreId <$> readTokens path
  SaveToken coreId token -> do
    tokens <- readTokens path
    writeTokens path (insert coreId token tokens)

-- | The conventional place for the tokens of an extension:
-- @$XDG_STATE_HOME\/roon-api\/\<extension id\>.json@.
defaultTokenFile :: FileSystem :> es => Text -> Eff es FilePath
defaultTokenFile extensionId = do
  directory <- getXdgDirectory XdgState "roon-api"
  pure (directory </> unpack extensionId <.> "json")

-- | Keep the tokens in memory, starting from the given ones. Returns the
-- tokens held at the end. Meant for tests.
runTokenStorePure :: Map Text Text -> Eff (TokenStore : es) a -> Eff es (a, Map Text Text)
runTokenStorePure initial = reinterpret_ (runState initial) $ \case
  LoadToken coreId -> gets (M.lookup coreId)
  SaveToken coreId token -> modify (insert coreId token)

readTokens :: FileSystem :> es => FilePath -> Eff es (Map Text Text)
readTokens path = do
  exists <- doesFileExist path
  if not exists
    then pure empty
    else do
      contents <- FS.readFile path
      pure . maybe empty id $
        decodeStrict contents >>= parseMaybe (withObject "token file" (.: "tokens"))

writeTokens :: FileSystem :> es => FilePath -> Map Text Text -> Eff es ()
writeTokens path tokens = do
  let directory = takeDirectory path
  createDirectoryIfMissing True directory
  -- 'openBinaryTempFile' creates the file with mode 0600.
  bracketOnError
    (openBinaryTempFile directory "tokens.tmp")
    (\(temporary, handle) -> hClose handle >> removeFile temporary)
    ( \(temporary, handle) -> do
        hPut handle (encode (object ["tokens" .= tokens]))
        hClose handle
        renameFile temporary path
    )

-- | Lifted 'IO.openBinaryTempFile', which "Effectful.FileSystem.IO" does
-- not provide.
openBinaryTempFile :: FileSystem :> es => FilePath -> String -> Eff es (FilePath, Handle)
openBinaryTempFile directory template = unsafeEff_ (IO.openBinaryTempFile directory template)
