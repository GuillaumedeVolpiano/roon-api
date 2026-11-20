-- |
-- Module      : Roon.Registry
-- Description : Introducing an extension to a Roon Server
-- License     : BSD-3-Clause
--
-- The registry service is the first thing an extension talks to. It tells
-- which server this is, then registers the extension. When the extension
-- asks for services and the server does not know it yet, the server only
-- answers the registration once the user has enabled the extension, in
-- Roon under Settings, Extensions. An extension that asks for no service
-- is registered at once.
module Roon.Registry
  ( -- * Describing the extension
    ExtensionInfo (..)

    -- * Describing the server
  , CoreInfo (..)
  , Registration (..)

    -- * Requests
  , info
  , register
  , registryService
  ) where

import Data.Aeson (FromJSON (..), Value, object, withObject, (.:), (.:?), (.=))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Effectful
import Effectful.Error.Static (Error, throwError)
import Effectful.Exception (finally)
import Roon.Effect.Moo (Moo, closeExchange, nextReply, openExchange, request)
import Roon.Error (RoonError (..))
import Roon.Moo.Message (Body (..))
import Roon.Service (expect)

-- | What an extension tells the server about itself. Roon shows the
-- display name, the version and the publisher to the user.
data ExtensionInfo = ExtensionInfo
  { extensionId :: Text
  -- ^ A stable, unique identifier, such as @com.example.my-extension@.
  -- The server ties its authorisation to it.
  , displayName :: Text
  , displayVersion :: Text
  , publisher :: Text
  , email :: Text
  , website :: Maybe Text
  , requiredServices :: [Text]
  -- ^ Services the server must offer, such as
  -- @com.roonlabs.transport:2@.
  , optionalServices :: [Text]
  , providedServices :: [Text]
  -- ^ Services the extension offers to the server.
  }
  deriving stock (Eq, Show)

-- | The identity of a Roon Server.
data CoreInfo = CoreInfo
  { coreId :: Text
  , displayName :: Text
  , displayVersion :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON CoreInfo where
  parseJSON = withObject "CoreInfo" $ \fields ->
    CoreInfo <$> fields .: "core_id" <*> fields .: "display_name" <*> fields .: "display_version"

-- | What the server answers to a successful registration.
data Registration = Registration
  { core :: CoreInfo
  , token :: Text
  -- ^ To present on later connections.
  , grantedServices :: [Text]
  -- ^ The services the server grants, among those asked for.
  }
  deriving stock (Eq, Show)

instance FromJSON Registration where
  parseJSON value =
    withObject
      "Registration"
      ( \fields ->
          Registration
            <$> parseJSON value
            <*> fields .: "token"
            <*> (fromMaybe [] <$> fields .:? "provided_services")
      )
      value

registryService :: Text
registryService = "com.roonlabs.registry:1"

-- | Ask the server who it is. Works before registration.
info :: (Moo :> es, Error RoonError :> es) => Eff es CoreInfo
info = do
  reply <- request name NoBody
  expect name "Success" reply
  where
    name = registryService <> "/info"

-- | Register the extension, with the token from an earlier registration
-- if there is one.
--
-- Without a valid token for the services asked for, this waits until the
-- user enables the extension in Roon.
register :: (Moo :> es, Error RoonError :> es) => ExtensionInfo -> Maybe Text -> Eff es Registration
register extension token = do
  -- The server answers with a first reply and may keep the exchange open;
  -- only that first reply matters here.
  exchange <- openExchange name (JsonBody (registrationRequest extension token))
  reply <- nextReply exchange `finally` closeExchange exchange
  maybe (throwError ConnectionLost) (expect name "Registered") reply
  where
    name = registryService <> "/register"

registrationRequest :: ExtensionInfo -> Maybe Text -> Value
registrationRequest extension token =
  object $
    [ "extension_id" .= extension.extensionId
    , "display_name" .= extension.displayName
    , "display_version" .= extension.displayVersion
    , "publisher" .= extension.publisher
    , "email" .= extension.email
    , "required_services" .= extension.requiredServices
    , "optional_services" .= extension.optionalServices
    , "provided_services" .= extension.providedServices
    ]
      <> catMaybes
        [ ("website" .=) <$> extension.website
        , ("token" .=) <$> token
        ]
