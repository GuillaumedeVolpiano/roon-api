-- |
-- Module      : Roon.Discovery
-- Description : Finding Roon Servers on the local network (SOOD)
-- License     : BSD-3-Clause
--
-- Roon Servers answer SOOD queries: UDP datagrams sent to port 9003, by
-- multicast to 239.255.90.90 and by broadcast. A query names the service
-- it looks for; each server answers with its name, version, unique id and
-- ports, to the port the query came from.
--
-- Observed with Roon Server 2.75: a server answers each query several
-- times, and its answers do not repeat the transaction id of the query.
-- Answers are told apart by the unique id of the server. A query without a
-- transaction id of its own was answered only the first time.
--
-- Queries go out through the default interface only.
module Roon.Discovery
  ( -- * Discovering
    Core (..)
  , DiscoveryOptions (..)
  , defaultDiscovery
  , discover

    -- * The SOOD format
  , SoodMessage (..)
  , encodeSood
  , decodeSood
  , roonServiceId
  , coreFromAnswer
  ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (nubBy)
import Data.Function (on)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Control.Monad (when)
import Effectful
import FlatParse.Basic qualified as FP
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket qualified as Socket
import Roon.Effect.WebSocket (ServerAddress (..))
import Streamly.Data.Stream qualified as Stream
import Streamly.External.ByteString qualified as StreamlyBS
import Streamly.External.Network.UDP qualified as Udp
import Text.Read (readMaybe)

-- | A SOOD datagram: its type (@Q@ for a query, @R@ for an answer) and its
-- properties, in order. A property may have no value.
data SoodMessage = SoodMessage
  { kind :: Char
  , properties :: [(Text, Maybe Text)]
  }
  deriving stock (Eq, Show)

-- | The service id of Roon Servers.
roonServiceId :: Text
roonServiceId = "00720724-5143-4a9b-abac-0e50cba674bb"

-- | Encode a datagram. Names longer than 255 bytes and values longer than
-- 65534 bytes are cut.
encodeSood :: SoodMessage -> ByteString
encodeSood SoodMessage {kind, properties} =
  "SOOD" <> ByteString.pack [2, fromIntegral (fromEnum kind)] <> foldMap property properties
  where
    property (name, value) =
      let nameBytes = ByteString.take 255 (encodeUtf8 name)
       in ByteString.singleton (fromIntegral (ByteString.length nameBytes)) <> nameBytes <> case value of
            Nothing -> ByteString.pack [0xff, 0xff]
            Just text ->
              let valueBytes = ByteString.take 65534 (encodeUtf8 text)
               in word16 (ByteString.length valueBytes) <> valueBytes
    word16 n = ByteString.pack [fromIntegral (n `shiftR` 8), fromIntegral (n .&. 0xff)]

-- | Decode a datagram. 'Nothing' if it is not SOOD version 2, or if a
-- property is cut short.
decodeSood :: ByteString -> Maybe SoodMessage
decodeSood bytes = case FP.runParser soodParser bytes of
  FP.OK message _ -> Just message
  _ -> Nothing

-- | A SOOD datagram, read to its end: the mark, the version, the type,
-- then properties, each a name with its length on one byte and a value
-- with its length on two, 0xffff standing for no value.
soodParser :: FP.Parser () SoodMessage
soodParser = do
  FP.byteString "SOOD"
  FP.word8 2
  kindByte <- FP.anyWord8
  properties <- FP.many property
  FP.eof
  pure SoodMessage {kind = toEnum (fromIntegral kindByte), properties}
  where
    property = do
      nameLength <- FP.anyWord8
      when (nameLength == 0) FP.failed
      name <- utf8 (FP.take (fromIntegral nameLength))
      valueLength <- FP.anyWord16be
      value <- if valueLength == 0xffff then pure Nothing else Just <$> utf8 (FP.take (fromIntegral valueLength))
      pure (name, value)
    utf8 parser = parser >>= either (const FP.failed) pure . decodeUtf8'

-- | A Roon Server found on the network.
data Core = Core
  { uniqueId :: Text
  -- ^ The same as the core id the registry reports.
  , name :: Maybe Text
  , displayVersion :: Maybe Text
  , address :: ServerAddress
  -- ^ Where its extension API listens.
  , properties :: [(Text, Maybe Text)]
  -- ^ Everything the answer said.
  }
  deriving stock (Eq, Show)

-- | The server an answer describes, from the address it came from. The
-- answer may name another address and port to reply to (@_replyaddr@,
-- @_replyport@); the extension API listens on @http_port@.
coreFromAnswer :: String -> SoodMessage -> Maybe Core
coreFromAnswer sender SoodMessage {kind, properties}
  | kind /= 'R' = Nothing
  | lookup' "service_id" /= Just roonServiceId = Nothing
  | otherwise = do
      uniqueId <- lookup' "unique_id"
      port <- lookup' "http_port" >>= readMaybe . Text.unpack
      let host = maybe sender Text.unpack (lookup' "_replyaddr")
      pure
        Core
          { uniqueId
          , name = lookup' "name"
          , displayVersion = lookup' "display_version"
          , address = ServerAddress {host, port}
          , properties
          }
  where
    lookup' key = case lookup key properties of
      Just (Just value) -> Just value
      _ -> Nothing

-- | Where to look for servers, and for how long.
data DiscoveryOptions = DiscoveryOptions
  { targets :: [(Socket.HostAddress, Socket.PortNumber)]
  -- ^ Where to send the query.
  , wait :: Int
  -- ^ Microseconds to collect answers.
  }
  deriving stock (Eq, Show)

-- | The SOOD multicast group and the broadcast address, port 9003; one
-- second for the answers.
defaultDiscovery :: DiscoveryOptions
defaultDiscovery =
  DiscoveryOptions
    { targets = [(Socket.tupleToHostAddress (239, 255, 90, 90), 9003), (Socket.tupleToHostAddress (255, 255, 255, 255), 9003)]
    , wait = 1_000_000
    }

-- | Query the network for Roon Servers and collect the answers for a
-- while. Each server is listed once, in the order of its first answer.
discover :: IOE :> es => DiscoveryOptions -> Eff es [Core]
discover options = liftIO . Udp.withSocket (Udp.socketAt Udp.anyAddress) {Udp.broadcast = True} $ \socket -> do
  -- A transaction id of its own for each query: with none, Roon Server
  -- 2.75 answered the first query only.
  transaction <- Text.pack . show <$> getMonotonicTimeNSec
  let query = StreamlyBS.toArray (encodeSood SoodMessage {kind = 'Q', properties = [("query_service_id", Just roonServiceId), ("_tid", Just transaction)]})
  Stream.fold (Udp.writeTo socket) (Stream.fromList [(Socket.SockAddrInet port host, query) | (host, port) <- options.targets])
  nubBy ((==) `on` (.uniqueId)) <$> Stream.toList (Stream.mapMaybe core (Udp.datagramsWithin options.wait socket))
  where
    core (sender, datagram) = decodeSood (StreamlyBS.fromArray datagram) >>= coreFromAnswer (hostOf sender)
    hostOf = \case
      Socket.SockAddrInet _ host -> let (a, b, c, d) = Socket.hostAddressToTuple host in foldr1 (\x y -> x <> "." <> y) (map show [a, b, c, d])
      other -> show other
