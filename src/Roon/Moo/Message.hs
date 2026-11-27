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

import Control.Monad (when)
import Data.Aeson (Value, eitherDecodeStrict)
import Data.Aeson qualified as J (encode)
import Data.ByteString (ByteString, breakSubstring, empty)
import Data.ByteString qualified as BS (drop, length, null, take)
import Data.ByteString.Builder (Builder, intDec, toLazyByteString)
import Data.ByteString.Builder qualified as BSB (byteString)
import Data.ByteString.Lazy (toStrict)
import Data.Map.Strict (Map, delete, foldMapWithKey, fromList)
import Data.Map.Strict qualified as M (lookup)
import Data.Text (Text, breakOn, pack, stripPrefix, unpack)
import Data.Text qualified as T (null)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Word (Word64)
import FlatParse.Basic (Parser, Pos (Pos), Result (Err, Fail, OK), anyWord8,
                        byteStringOf, cut, eof, failed, fails, getPos, many,
                        runParser, skipMany, skipSatisfyAscii, skipSome, word8)
import FlatParse.Basic qualified as FB (byteString)
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
requestIdFromWord = RequestId . pack . show

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
splitServiceMethod full = case breakOn "/" full of
  (service, rest)
    | not (T.null service), Just method <- stripPrefix "/" rest ->
        Just (service, method)
  _ -> Nothing

-- | Serialise a message into the bytes of one WebSocket binary frame.
encode :: MooMessage -> ByteString
encode msg =
  toStrict . toLazyByteString $
    mconcat
      [ "MOO/1 "
      , verbBuilder msg.verb
      , " "
      , text msg.name
      , "\n"
      , header "Request-Id" (let RequestId rid = msg.requestId in rid)
      , foldMapWithKey header msg.headers
      , contentHeaders
      , "\n"
      , BSB.byteString payload
      ]
  where
    text = BSB.byteString . encodeUtf8
    header key value = text key <> ": " <> text value <> "\n"
    (contentType, payload) = case msg.body of
      NoBody -> (Nothing, empty)
      JsonBody value -> (Just jsonContentType, toStrict (J.encode value))
      RawBody ctype bytes -> (Just ctype, bytes)
    contentHeaders = case contentType of
      Nothing -> mempty
      Just ctype ->
        "Content-Length: "
          <> intDec (BS.length payload)
          <> "\n"
          <> header "Content-Type" ctype

verbBuilder :: Verb -> Builder
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
  let (headerBlock, rest) = breakSubstring "\n\n" frame
  when (BS.null rest) (Left MissingHeaderEnd)
  let payload = BS.drop 2 rest
  -- Keep the line feed that ends the last header line, so that the block
  -- is a sequence of terminated lines.
  let headerLines = BS.take (BS.length headerBlock + 1) frame
  (verbName, name, headerList) <-
    case runParser headerBlockParser headerLines of
      OK parsed _ -> Right parsed
      -- The position of an error is counted from the end of the input.
      Err (Pos fromEnd, reason) -> Left (MalformedHeader (BS.length headerLines - fromEnd) reason)
      Fail -> Left (MalformedHeader 0 "not a MOO header block")
  verb <- case verbName of
    "REQUEST" -> Right Request
    "CONTINUE" -> Right Continue
    "COMPLETE" -> Right Complete
    other -> Left (UnknownVerb other)
  let allHeaders = fromList headerList
      headers = foldr delete allHeaders ["Request-Id", "Content-Length", "Content-Type"]
      contentType = M.lookup "Content-Type" allHeaders
  requestId <- maybe (Left MissingRequestId) (Right . RequestId) (M.lookup "Request-Id" allHeaders)
  contentLength <- case M.lookup "Content-Length" allHeaders of
    Nothing -> Right Nothing
    Just raw -> case readMaybe (unpack raw) of
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
          either (Left . InvalidJson) (Right . JsonBody) (eitherDecodeStrict (BS.take n payload))
      | otherwise -> Right (RawBody ctype (BS.take n payload))
  pure MooMessage {verb, name, requestId, headers, body}

-- | The first line and the header lines, each ended by a line feed.
-- Yields the verb, the name and the headers in order. A line that is not
-- what it should be is an error, with the position where it starts.
headerBlockParser :: Parser (Pos, String) (Text, Text, [(Text, Text)])
headerBlockParser = do
  (verbName, name) <- wholeLine "the first line is not MOO/1, a verb in capitals and a name" $ do
    FB.byteString "MOO/1 "
    verbName <- utf8 (byteStringOf (skipSome (skipSatisfyAscii (\char -> char >= 'A' && char <= 'Z'))))
    word8 space
    name <- restOfLine
    pure (verbName, name)
  headerList <- many (fails eof *> wholeLine "a header line is not a key, a colon and a value" headerLine)
  eof
  pure (verbName, name, headerList)
  where
    -- @Key: value@. The spaces after the colon are optional.
    headerLine = do
      key <- utf8 (byteStringOf (skipSome (notByte colon)))
      word8 colon
      skipMany (word8 space)
      value <- restOfLine
      pure (key, value)
    restOfLine = utf8 (byteStringOf (skipMany (notByte lineFeed))) <* word8 lineFeed
    -- A line, or an error that names what is wrong and where it starts.
    wholeLine problem content = do
      start <- getPos
      content `cut` (start, problem)
    utf8 bytesParser = bytesParser >>= either (const failed) pure . decodeUtf8'
    notByte excluded = anyWord8 >>= \byte -> if byte == excluded || byte == lineFeed then failed else pure ()
    space = 32
    colon = 58
    lineFeed = 10
