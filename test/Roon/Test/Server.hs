-- | A scripted Roon Server for the tests: fixtures, the queues that stand
-- for the connection, and the steps a script is made of.
module Roon.Test.Server
  ( -- * Fixtures
    extension
  , testCore
  , coreBody
  , registered

    -- * The wire
  , Wire (..)
  , newWire
  , withServer

    -- * Script steps
  , receiveFromClient
  , sendToClient
  , closeConnection
  , expectRequest
  , answer
  , serverRequest
  , serveInfo
  , field
  ) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Roon.Effect.TokenStore (TokenStore, runTokenStorePure)
import Roon.Effect.WebSocket (WebSocket, runWebSocketWith)
import Roon.Error (RoonError)
import Roon.Moo.Message
import Roon.Registry
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=))

-- Fixtures

extension :: ExtensionInfo
extension =
  ExtensionInfo
    { extensionId = "test.extension"
    , displayName = "Test extension"
    , displayVersion = "0.0.1"
    , publisher = "roon-api tests"
    , email = "nobody@example.invalid"
    , website = Nothing
    , requiredServices = ["com.roonlabs.transport:2"]
    , optionalServices = []
    , providedServices = []
    }

testCore :: CoreInfo
testCore = CoreInfo {coreId = "core-1", displayName = "Test Core", displayVersion = "2.0 (build 1)"}

coreBody :: Body
coreBody =
  JsonBody . object $
    [ "core_id" .= ("core-1" :: Text)
    , "display_name" .= ("Test Core" :: Text)
    , "display_version" .= ("2.0 (build 1)" :: Text)
    ]

registered :: Text -> Body
registered token =
  JsonBody . object $
    [ "core_id" .= ("core-1" :: Text)
    , "display_name" .= ("Test Core" :: Text)
    , "display_version" .= ("2.0 (build 1)" :: Text)
    , "token" .= token
    , "provided_services" .= ["com.roonlabs.transport:2" :: Text]
    , "http_port" .= (9330 :: Int)
    ]

-- The connection, as seen from the server

data Wire = Wire
  { toServer :: TQueue ByteString
  , toClient :: TQueue (Maybe ByteString)
  -- ^ 'Nothing' closes the connection.
  }

newWire :: IO Wire
newWire = Wire <$> newTQueueIO <*> newTQueueIO

-- The in-memory transport

type ClientEffects = '[WebSocket, TokenStore, Error RoonError, Concurrent, IOE]

-- | Run a client against a server script. Returns what the client
-- produced, with the tokens it holds at the end, and what the script
-- returned.
withServer
  :: Map Text Text
  -> Eff ClientEffects a
  -> (Wire -> IO b)
  -> IO (Either RoonError (a, Map Text Text), b)
withServer tokens client server = do
  wire <- newWire
  let sendAction frame = liftIO (True <$ atomically (writeTQueue wire.toServer frame))
      receiveAction = liftIO (atomically (readTQueue wire.toClient))
      runClient =
        runEff . runConcurrent . runErrorNoCallStack . runTokenStorePure tokens $
          runWebSocketWith sendAction receiveAction client
  withAsync (server wire) $ \serverThread -> do
    outcome <- runClient
    result <- wait serverThread
    pure (outcome, result)

receiveFromClient :: Wire -> IO MooMessage
receiveFromClient wire = do
  frame <- atomically (readTQueue wire.toServer)
  either (\err -> assertFailure ("the client sent a malformed frame: " <> show err)) pure (parse frame)

sendToClient :: Wire -> MooMessage -> IO ()
sendToClient wire = atomically . writeTQueue wire.toClient . Just . encode

closeConnection :: Wire -> IO ()
closeConnection wire = atomically (writeTQueue wire.toClient Nothing)

-- | Wait for a request and check its name.
expectRequest :: Wire -> Text -> IO MooMessage
expectRequest wire name = do
  message <- receiveFromClient wire
  (message.verb, message.name) @?= (Request, name)
  pure message

-- | Reply to a request of the client.
answer :: Wire -> MooMessage -> Verb -> Text -> Body -> IO ()
answer wire to verb name body =
  sendToClient wire MooMessage {verb, name, requestId = to.requestId, headers = Map.empty, body}

-- | A request made by the server.
serverRequest :: Text -> Text -> MooMessage
serverRequest requestId name =
  MooMessage {verb = Request, name, requestId = RequestId requestId, headers = Map.empty, body = NoBody}

-- | Answer the info request that opens every registration.
serveInfo :: Wire -> IO ()
serveInfo wire = do
  infoRequest <- expectRequest wire "com.roonlabs.registry:1/info"
  assertBool "info carries no body" (infoRequest.body == NoBody)
  answer wire infoRequest Complete "Success" coreBody

-- | A field of the JSON body of a message.
field :: Text -> MooMessage -> Maybe Value
field key message = case message.body of
  JsonBody (Object fields) -> KeyMap.lookup (Key.fromText key) fields
  _ -> Nothing
