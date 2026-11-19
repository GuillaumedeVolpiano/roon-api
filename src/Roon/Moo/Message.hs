-- |
-- Module      : Roon.Moo.Message
-- Description : The MOO message format used by the Roon extension API
-- License     : BSD-3-Clause
--
-- MOO is the request and response protocol that a Roon Server and its
-- extensions speak over a WebSocket. Each WebSocket binary frame carries
-- exactly one message:
--
-- > MOO/1 REQUEST com.roonlabs.registry:1/info
-- > Request-Id: 0
-- > Content-Length: 2
-- > Content-Type: application/json
-- >
-- > {}
--
-- The first line gives the protocol version, a 'Verb' and a name. Header
-- lines follow, then an empty line, then an optional body whose size is
-- given by @Content-Length@. Lines end with a single line feed.
module Roon.Moo.Message
  ( -- * Messages
    MooMessage (..)
  , Verb (..)
  , RequestId (..)
  , Body (..)

    -- * Encoding and parsing
  , encode
  , parse
  , MooParseError (..)

    -- * Helpers
  , requestIdFromWord
  , splitServiceMethod
  ) where

import Control.Monad (void, when)
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BSL
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word64, Word8)
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Parser (Parser)
import Streamly.Data.Parser qualified as Parser
import Streamly.Data.Stream qualified as Stream
import Streamly.External.ByteString qualified as StreamlyBS
import Text.Read (readMaybe)

-- | The kind of a MOO message.
data Verb
  = -- | Opens an exchange. The message name is @service/method@.
    Request
  | -- | An intermediate reply; more will follow for the same request.
    Continue
  | -- | The final reply to a request.
    Complete
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Identifies an exchange. Replies carry the identifier of the request
-- they answer. Roon uses decimal counters, but nothing in the protocol
-- requires it, so the identifier is kept as received.
newtype RequestId = RequestId Text
  deriving stock (Eq, Ord, Show)

-- | The identifier for the n-th request sent on a connection.
requestIdFromWord :: Word64 -> RequestId
requestIdFromWord = RequestId . Text.pack . show

-- | The payload of a message.
data Body
  = NoBody
  | -- | Sent and recognised with the @application/json@ content type.
    JsonBody Value
  | -- | Any other content type, with its bytes. Never empty: an empty
    -- payload is 'NoBody'.
    RawBody Text ByteString
  deriving stock (Eq, Show)

-- | One MOO message.
data MooMessage = MooMessage
  { verb :: Verb
  , name :: Text
  -- ^ @service/method@ for a 'Request'; a status such as @Success@ for
  -- 'Continue' and 'Complete'.
  , requestId :: RequestId
  , headers :: Map Text Text
  -- ^ Headers other than @Request-Id@, @Content-Length@ and
  -- @Content-Type@, which are represented by the other fields.
  , body :: Body
  }
  deriving stock (Eq, Show)

-- | Why a frame is not a valid MOO message.
data MooParseError
  = -- | No empty line ends the header block.
    MissingHeaderEnd
  | -- | The header block is malformed, at this byte offset.
    MalformedHeader Int String
  | UnknownVerb Text
  | MissingRequestId
  | InvalidContentLength Text
  | -- | A @Content-Type@ without a @Content-Length@.
    ContentTypeWithoutLength
  | -- | A body without a @Content-Type@.
    MissingContentType
  | -- | Bytes after the headers although no @Content-Length@ announced
    -- them.
    UnexpectedBody
  | -- | Fewer body bytes than announced: expected, then received.
    TruncatedBody Int Int
  | InvalidJson String
  deriving stock (Eq, Show)

-- | Split the name of a 'Request' into its service and its method, at the
-- first slash.
--
-- >>> splitServiceMethod "com.roonlabs.ping:1/ping"
-- Just ("com.roonlabs.ping:1","ping")
splitServiceMethod :: Text -> Maybe (Text, Text)
splitServiceMethod full = case Text.breakOn "/" full of
  (service, rest)
    | not (Text.null service), Just method <- Text.stripPrefix "/" rest ->
        Just (service, method)
  _ -> Nothing

-- | Serialise a message into the bytes of one WebSocket binary frame.
encode :: MooMessage -> ByteString
encode msg =
  BSL.toStrict . Builder.toLazyByteString $
    mconcat
      [ "MOO/1 "
      , verbBuilder msg.verb
      , " "
      , text msg.name
      , "\n"
      , header "Request-Id" (let RequestId rid = msg.requestId in rid)
      , Map.foldMapWithKey header msg.headers
      , contentHeaders
      , "\n"
      , Builder.byteString payload
      ]
  where
    text = Builder.byteString . Text.encodeUtf8
    header key value = text key <> ": " <> text value <> "\n"
    (contentType, payload) = case msg.body of
      NoBody -> (Nothing, BS.empty)
      JsonBody value -> (Just jsonContentType, BSL.toStrict (Aeson.encode value))
      RawBody ctype bytes -> (Just ctype, bytes)
    contentHeaders = case contentType of
      Nothing -> mempty
      Just ctype ->
        "Content-Length: "
          <> Builder.intDec (BS.length payload)
          <> "\n"
          <> header "Content-Type" ctype

verbBuilder :: Verb -> Builder.Builder
verbBuilder = \case
  Request -> "REQUEST"
  Continue -> "CONTINUE"
  Complete -> "COMPLETE"

jsonContentType :: Text
jsonContentType = "application/json"

-- | Parse the bytes of one WebSocket frame.
--
-- Like the reference implementation, this ignores bytes beyond
-- @Content-Length@ and keeps the last occurrence of a repeated header.
parse :: ByteString -> Either MooParseError MooMessage
parse frame = do
  let (headerBlock, rest) = BS.breakSubstring "\n\n" frame
  when (BS.null rest) (Left MissingHeaderEnd)
  let payload = BS.drop 2 rest
  -- Keep the line feed that ends the last header line, so that the block
  -- is a sequence of terminated lines.
  let headerLines = BS.take (BS.length headerBlock + 1) frame
  (verbName, name, headerList) <-
    case runIdentity (Stream.parsePos headerBlockParser (Stream.unfold StreamlyBS.reader headerLines)) of
      Left (Parser.ParseErrorPos offset reason) -> Left (MalformedHeader offset reason)
      Right parsed -> Right parsed
  verb <- case verbName of
    "REQUEST" -> Right Request
    "CONTINUE" -> Right Continue
    "COMPLETE" -> Right Complete
    other -> Left (UnknownVerb other)
  let allHeaders = Map.fromList headerList
      headers = foldr Map.delete allHeaders ["Request-Id", "Content-Length", "Content-Type"]
      contentType = Map.lookup "Content-Type" allHeaders
  requestId <- maybe (Left MissingRequestId) (Right . RequestId) (Map.lookup "Request-Id" allHeaders)
  contentLength <- case Map.lookup "Content-Length" allHeaders of
    Nothing -> Right Nothing
    Just raw -> case readMaybe (Text.unpack raw) of
      Just n | n >= 0 -> Right (Just n)
      _ -> Left (InvalidContentLength raw)
  body <- case (contentLength, contentType) of
    (Nothing, Just _) -> Left ContentTypeWithoutLength
    (Nothing, Nothing)
      | BS.null payload -> Right NoBody
      | otherwise -> Left UnexpectedBody
    (Just 0, _) -> Right NoBody
    (Just _, Nothing) -> Left MissingContentType
    (Just n, Just ctype)
      | BS.length payload < n -> Left (TruncatedBody n (BS.length payload))
      | ctype == jsonContentType ->
          either (Left . InvalidJson) (Right . JsonBody) (Aeson.eitherDecodeStrict (BS.take n payload))
      | otherwise -> Right (RawBody ctype (BS.take n payload))
  pure MooMessage {verb, name, requestId, headers, body}

-- | The first line and the header lines, each ended by a line feed.
-- Yields the verb, the name and the headers in order.
--
-- Every line must be terminated, including the last one. With
-- streamly-core 0.3.3, if the input ends while the parser repeated by
-- 'Parser.many' is part-way through a line, what it consumed is dropped
-- and a following 'Parser.eof' succeeds: an unterminated, malformed last
-- line would go unnoticed.
headerBlockParser :: Parser Word8 Identity (Text, Text, [(Text, Text)])
headerBlockParser = do
  void (Parser.listEq (BS.unpack "MOO/1 "))
  verbName <- utf8 (Parser.takeWhile1 isUpperAscii Fold.toList)
  void (Parser.satisfy (== space))
  name <- restOfLine
  headerList <- Parser.many headerLine Fold.toList
  Parser.eof
  pure (verbName, name, headerList)
  where
    -- @Key: value@. The spaces after the colon are optional.
    headerLine = do
      key <- utf8 (Parser.takeWhile1 (\byte -> byte /= colon && byte /= lineFeed) Fold.toList)
      void (Parser.satisfy (== colon))
      Parser.dropWhile (== space)
      value <- restOfLine
      pure (key, value)
    restOfLine = utf8 (Parser.takeWhile (/= lineFeed) Fold.toList) <* Parser.satisfy (== lineFeed)
    utf8 bytesParser = do
      bytes <- bytesParser
      case Text.decodeUtf8' (BS.pack bytes) of
        Left _ -> Parser.die "invalid UTF-8"
        Right decoded -> pure decoded
    isUpperAscii byte = byte >= 65 && byte <= 90
    space = 32
    colon = 58
    lineFeed = 10
