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

import Control.Monad (when)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString, singleton)
import Data.ByteString qualified as BS (length, pack, take)
import Data.Function (on)
import Data.List (nubBy)
import Data.Text (Text, unpack)
import Data.Text qualified as T (pack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Effectful (Eff, IOE, MonadIO (liftIO), type (:>))
import FlatParse.Basic (Parser, Result (OK), anyWord16be, anyWord8, byteString,
                        eof, failed, many, runParser, word8)
import FlatParse.Basic qualified as FB (take)
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket (HostAddress, PortNumber, SockAddr (SockAddrInet),
                       hostAddressToTuple, tupleToHostAddress)
import Roon.Effect.WebSocket (ServerAddress (..))
import Streamly.Data.Stream (fold, fromList, mapMaybe, toList)
import Streamly.External.ByteString (fromArray, toArray)
import Streamly.External.Network.UDP (SocketConfig (broadcast), anyAddress,
                                      datagramsWithin, socketAt, withSocket,
                                      writeTo)
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
  "SOOD" <> BS.pack [2, fromIntegral (fromEnum kind)] <> foldMap property properties
  where
    property (name, value) =
      let nameBytes = BS.take 255 (encodeUtf8 name)
       in singleton (fromIntegral (BS.length nameBytes)) <> nameBytes <> case value of
            Nothing -> BS.pack [0xff, 0xff]
            Just text ->
              let valueBytes = BS.take 65534 (encodeUtf8 text)
               in word16 (BS.length valueBytes) <> valueBytes
    word16 n = BS.pack [fromIntegral (n `shiftR` 8), fromIntegral (n .&. 0xff)]

-- | Decode a datagram. 'Nothing' if it is not SOOD version 2, or if a
-- property is cut short.
decodeSood :: ByteString -> Maybe SoodMessage
decodeSood bytes = case runParser soodParser bytes of
  OK message _ -> Just message
  _ -> Nothing

-- | A SOOD datagram, read to its end: the mark, the version, the type,
-- then properties, each a name with its length on one byte and a value
-- with its length on two, 0xffff standing for no value.
soodParser :: Parser () SoodMessage
soodParser = do
  byteString "SOOD"
  word8 2
  kindByte <- anyWord8
  properties <- many property
  eof
  pure SoodMessage {kind = toEnum (fromIntegral kindByte), properties}
  where
    property = do
      nameLength <- anyWord8
      when (nameLength == 0) failed
      name <- utf8 (FB.take (fromIntegral nameLength))
      valueLength <- anyWord16be
      value <- if valueLength == 0xffff then pure Nothing else Just <$> utf8 (FB.take (fromIntegral valueLength))
      pure (name, value)
    utf8 parser = parser >>= either (const failed) pure . decodeUtf8'

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
      port <- lookup' "http_port" >>= readMaybe . unpack
      let host = maybe sender unpack (lookup' "_replyaddr")
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
  { targets :: [(HostAddress, PortNumber)]
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
    { targets = [(tupleToHostAddress (239, 255, 90, 90), 9003), (tupleToHostAddress (255, 255, 255, 255), 9003)]
    , wait = 1_000_000
    }

-- | Query the network for Roon Servers and collect the answers for a
-- while. Each server is listed once, in the order of its first answer.
discover :: IOE :> es => DiscoveryOptions -> Eff es [Core]
discover options = liftIO . withSocket (socketAt anyAddress) {broadcast = True} $ \socket -> do
  -- A transaction id of its own for each query: with none, Roon Server
  -- 2.75 answered the first query only.
  transaction <- T.pack . show <$> getMonotonicTimeNSec
  let query = toArray (encodeSood SoodMessage {kind = 'Q', properties = [("query_service_id", Just roonServiceId), ("_tid", Just transaction)]})
  fold (writeTo socket) (fromList [(SockAddrInet port host, query) | (host, port) <- options.targets])
  nubBy ((==) `on` (.uniqueId)) <$> toList (mapMaybe core (datagramsWithin options.wait socket))
  where
    core (sender, datagram) = decodeSood (fromArray datagram) >>= coreFromAnswer (hostOf sender)
    hostOf = \case
      SockAddrInet _ host -> let (a, b, c, d) = hostAddressToTuple host in foldr1 (\x y -> x <> "." <> y) (map show [a, b, c, d])
      other -> show other
