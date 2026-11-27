-- | How fast the parsers of the hot paths are, against other ways to
-- write them: the header block of a MOO message, the head of an HTTP
-- response, and the JSON body of a list of zones. Measured with
-- tasty-bench; the checks at the end make sure the ways compared agree.
--
-- The zones come from the Roon Server of this machine when it answers.
module Main (main) where

import Control.Applicative ((<|>))
import Control.DeepSeq (force)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (KeyValue ((.=)), Value (..), decodeStrict, object)
import Data.Aeson qualified as J (encode)
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as JK (fromList, lookup)
import Data.Aeson.Types (parseEither, parseJSON)
import Data.ByteString (ByteString, breakSubstring, dropWhileEnd, pack,
                        toStrict, unpack)
import Data.ByteString qualified as BS (break, concat, drop, dropWhile, length,
                                        map, take)
import Data.ByteString.Char8 qualified as BS8 (lines, map)
import Data.Char (chr, isDigit)
import Data.Functor.Identity (Identity (..))
import Data.List qualified as L (foldl')
import Data.Map.Strict (empty)
import Data.Scientific (scientific)
import Data.Text (Text, singleton)
import Data.Text.Encoding (decodeUtf8', decodeUtf8Lenient, encodeUtf8)
import Data.Vector qualified as V (fromList)
import Data.Word (Word8)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.FileSystem (runFileSystem)
import FlatParse.Basic (Result (OK), anyAsciiDecimalInt, anyAsciiDecimalInteger,
                        anyAsciiHexInt, anyWord8, byteString, byteStringOf,
                        failed, isolate, runParser, skipMany, skipSatisfyAscii,
                        skipSome, withByteString, word8)
import FlatParse.Basic qualified as FB (Parser, eof, many)
import Roon.Connection (withRoon)
import Roon.Effect.Moo (Reply (..), pingService, request)
import Roon.Effect.TokenStore (defaultTokenFile, runTokenStoreFile)
import Roon.Effect.WebSocket (localServer)
import Roon.Error (RoonError)
import Roon.Moo.Message (Body (..), MooMessage (..), RequestId (..), Verb (..))
import Roon.Moo.Message qualified as MM (encode, parse)
import Roon.Registry (ExtensionInfo (..))
import Roon.Transport (Zone)
import Streamly.Data.Array (toParserK)
import Streamly.Data.Array qualified as A (parse)
import Streamly.Data.Fold (toList)
import Streamly.Data.Parser (Parser, die, listEq, satisfy, takeWhile1)
import Streamly.Data.Parser qualified as P (dropWhile, eof, many, takeWhile)
import Streamly.Data.Stream (unfold)
import Streamly.Data.Stream qualified as S (parse)
import Streamly.Data.StreamK (fromPure)
import Streamly.External.ByteString (reader, toArray)
import Streamly.External.Network.HTTP.Wire (Response (..),
                                            readResponseHeadWithin,
                                            responseHead)
import Test.Tasty.Bench (bench, bgroup, defaultMain, nf, nfIO)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Text.Printf (printf)

main :: IO ()
main = do
  (source, zonesValue) <- zonesBody
  let zonesJson = toStrict (J.encode zonesValue)
      frame = MM.encode MooMessage {verb = Complete, name = "Success", requestId = RequestId "42", headers = empty, body = JsonBody zonesValue}
      block = BS.take (BS.length (fst (breakSubstring "\n\n" frame)) + 1) frame
  printf "Zones body: %s, %d bytes. MOO header block: %d bytes. HTTP head: %d bytes.\n\n" source (BS.length zonesJson) (BS.length block) (BS.length httpHead)
  defaultMain
    [ bgroup
        "MOO header block"
        [ bench "Streamly parser, monadic (as Roon.Moo.Message was)" (nf (run mooMonadic) block)
        , bench "Streamly parser, applicative" (nf (run mooApplicative) block)
        , bench "flatparse (as Roon.Moo.Message is)" (nf (flat mooFlat) block)
        , bench "ByteString splitting by hand (a floor, not a proposal)" (nf mooByHand block)
        ]
    , bgroup
        "MOO message, whole"
        [ bench "Roon.Moo.Message.parse" (nf (either (const Nothing) (Just . whole) . MM.parse) frame)
        ]
    , bgroup
        "HTTP response head"
        [ bench "streamly-http as it is (flatparse, through streamly-flatparse)" $
            nfIO (summary . fst <$> readResponseHeadWithin (Just 65536) (fromPure (toArray httpHead)))
        , bench "streamly-http as it was (Streamly parser over arrays, ParserK)" $
            nf (either (const Nothing) Just . runIdentity . A.parse (toParserK streamlyHead) . fromPure . toArray) httpHead
        , bench "the Streamly parser over a stream of bytes" (nf (run streamlyHead) httpHead)
        , bench "a flatparse parser written for this benchmark" (nf (flat httpFlat) httpHead)
        ]
    , bgroup
        "JSON body of the zones"
        [ bench "aeson, to Value" (nf (decodeStrict :: ByteString -> Maybe Value) zonesJson)
        , bench "flatparse, to Value" (nf (flat (jsonFlat <* FB.eof)) zonesJson)
        , -- Zone has no NFData instance: the zones are shown to force them,
          -- which adds to these two times alike.
          bench "aeson, to Value, then FromJSON to [Zone]" (nf (fmap (length . show) . toZones) zonesJson)
        , bench "FromJSON to [Zone] alone, from a Value" (nf (fmap (length . show) . fromValue) zonesValue)
        ]
    , bgroup
        "the ways compared agree"
        [ testCase "MOO header block" $
            assertBool "differ" (all (== run mooMonadic block) [run mooApplicative block, flat mooFlat block, Just (mooByHand block)])
        , testCase "HTTP head" $ do
            fmap summary (flat (responseHead <* FB.eof) httpHead) @?= flat httpFlat httpHead
            run streamlyHead httpHead @?= flat httpFlat httpHead
        , testCase "JSON" $
            flat (jsonFlat <* FB.eof) zonesJson @?= decodeStrict zonesJson
        ]
    ]
  where
    run :: Parser Word8 Identity a -> ByteString -> Maybe a
    run parser bytes = either (const Nothing) Just (runIdentity (S.parse parser (unfold reader bytes)))
    flat :: FB.Parser e a -> ByteString -> Maybe a
    flat parser bytes = case runParser parser bytes of
      OK found _ -> Just found
      _ -> Nothing
    whole message = (message.name, message.headers, case message.body of JsonBody value -> value; _ -> Null)
    summary response = (response.status, response.reason, response.headers)
    toZones bytes = decodeStrict bytes >>= fromValue
    fromValue :: Value -> Maybe [Zone]
    fromValue = \case
      Object fields | Just zones <- JK.lookup "zones" fields -> either (const Nothing) Just (parseEither parseJSON zones)
      _ -> Nothing

-- Inputs

-- | The zones of the Roon Server of this machine, or made-up ones when it
-- cannot be reached.
zonesBody :: IO (String, Value)
zonesBody = do
  tokenFile <- runEff (runFileSystem (defaultTokenFile extension.extensionId))
  outcome <- try . runEff . runConcurrent . runFileSystem . runErrorNoCallStack @RoonError . runTokenStoreFile tokenFile . withRoon localServer extension $ \_ ->
    request "com.roonlabs.transport:2/get_zones" NoBody
  pure $ case outcome of
    Right (Right Reply {body = JsonBody value}) -> ("from the Roon Server of this machine", force value)
    Right _ -> ("made up (the server gave no zones)", madeUp)
    Left (_ :: SomeException) -> ("made up (no server reached)", madeUp)
  where
    madeUp = object ["zones" .= ([] :: [Value])]
    extension =
      ExtensionInfo
        { extensionId = "io.github.guillaumedevolpiano.roon-api.probe"
        , displayName = "roon-api probe"
        , displayVersion = "bench"
        , publisher = "roon-api"
        , email = "nobody@example.invalid"
        , website = Nothing
        , requiredServices = ["com.roonlabs.transport:2", "com.roonlabs.browse:1"]
        , optionalServices = []
        , providedServices = [pingService]
        }

-- | The head of an image response, as Roon Server 2.75 sent it.
httpHead :: ByteString
httpHead =
  "HTTP/1.1 200 OK\r\nAccess-Control-Allow-Origin: *\r\nEtag: 96f45bde867a0aaf6df5ff2f47526aa6916ed3ff:6a8027a650750256c151c53e7ee9017f\r\n\
  \Accept-Ranges: bytes\r\nCache-Control: max-age=21600, public\r\nContent-Type: image/jpeg\r\nContent-Length: 43183\r\n\r\n"

-- The MOO header block, four ways

-- | As in Roon.Moo.Message.
mooMonadic :: Parser Word8 Identity (Text, Text, [(Text, Text)])
mooMonadic = do
  void (listEq (unpack "MOO/1 "))
  verbName <- utf8 (takeWhile1 isUpperAscii toList)
  void (satisfy (== 32))
  name <- restOfLine
  headerList <- P.many headerLine toList
  P.eof
  pure (verbName, name, headerList)
  where
    headerLine = do
      key <- utf8 (takeWhile1 (\byte -> byte /= 58 && byte /= 10) toList)
      void (satisfy (== 58))
      P.dropWhile (== 32)
      value <- restOfLine
      pure (key, value)
    restOfLine = utf8 (P.takeWhile (/= 10) toList) <* satisfy (== 10)
    utf8 bytesParser = do
      bytes <- bytesParser
      case decodeUtf8' (pack bytes) of
        Left _ -> die "invalid UTF-8"
        Right decoded -> pure decoded
    isUpperAscii byte = byte >= 65 && byte <= 90

-- | The same grammar without any bind: UTF-8 is decoded leniently, since
-- deciding to fail on a decoded value needs a bind.
mooApplicative :: Parser Word8 Identity (Text, Text, [(Text, Text)])
mooApplicative =
  (,,)
    <$> (listEq (unpack "MOO/1 ") *> text (takeWhile1 isUpperAscii toList) <* satisfy (== 32))
    <*> restOfLine
    <*> P.many headerLine toList
    <* P.eof
  where
    headerLine =
      (,)
        <$> (text (takeWhile1 (\byte -> byte /= 58 && byte /= 10) toList) <* satisfy (== 58) <* P.dropWhile (== 32))
        <*> restOfLine
    restOfLine = text (P.takeWhile (/= 10) toList) <* satisfy (== 10)
    text = fmap (decodeUtf8Lenient . pack)
    isUpperAscii byte = byte >= 65 && byte <= 90

mooFlat :: FB.Parser () (Text, Text, [(Text, Text)])
mooFlat = do
  byteString "MOO/1 "
  verbName <- utf8 (byteStringOf (skipSome (skipSatisfyAscii (\c -> c >= 'A' && c <= 'Z'))))
  word8 32
  name <- restOfLine
  headerList <- FB.many headerLine
  FB.eof
  pure (verbName, name, headerList)
  where
    headerLine = do
      key <- utf8 (byteStringOf (skipSome (notByte2 58 10)))
      word8 58
      skipMany (word8 32)
      value <- restOfLine
      pure (key, value)
    restOfLine = utf8 (byteStringOf (skipMany (notByte 10))) <* word8 10
    utf8 parser = parser >>= either (const failed) pure . decodeUtf8'

-- | Any byte but one.
notByte :: Word8 -> FB.Parser e ()
notByte excluded = anyWord8 >>= \byte -> if byte == excluded then failed else pure ()

notByte2 :: Word8 -> Word8 -> FB.Parser e ()
notByte2 one other = anyWord8 >>= \byte -> if byte == one || byte == other then failed else pure ()

-- | What the same block gives when its lines are split by hand: no error
-- reporting, no check of the grammar.
mooByHand :: ByteString -> (Text, Text, [(Text, Text)])
mooByHand block = case BS8.lines block of
  first : rest ->
    let (verbName, name) = BS.break (== 32) (BS.drop 6 first)
     in (decodeUtf8Lenient verbName, decodeUtf8Lenient (BS.drop 1 name), map header rest)
  [] -> ("", "", [])
  where
    header line =
      let (key, value) = BS.break (== 58) line
       in (decodeUtf8Lenient key, decodeUtf8Lenient (BS.dropWhile (== 32) (BS.drop 1 value)))

-- The HTTP head

-- | The parser of streamly-http before it moved to flatparse.
streamlyHead :: Parser Word8 Identity (Int, ByteString, [(ByteString, ByteString)])
streamlyHead = do
  void (listEq (unpack "HTTP/1."))
  void (satisfy isDigitByte)
  void (satisfy (== 32))
  digits <- takeWhile1 isDigitByte toList
  reason <- restOfLine
  found <- P.many headerLine toList
  P.dropWhile (== 13)
  void (satisfy (== 10))
  pure (L.foldl' (\total byte -> total * 10 + fromIntegral (byte - 48)) 0 digits, reason, found)
  where
    headerLine = do
      name <- takeWhile1 (\byte -> byte > 32 && byte < 127 && byte /= 58) toList
      void (satisfy (== 58))
      value <- restOfLine
      pure (BS.map (\byte -> if byte >= 65 && byte <= 90 then byte + 32 else byte) (pack name), value)
    restOfLine = do
      bytes <- P.takeWhile (/= 10) toList
      void (satisfy (== 10))
      pure (BS.dropWhile blank (dropWhileEnd blank (pack bytes)))
    blank byte = byte == 32 || byte == 9 || byte == 13
    isDigitByte byte = byte >= 48 && byte <= 57

httpFlat :: FB.Parser () (Int, ByteString, [(ByteString, ByteString)])
httpFlat = do
  byteString "HTTP/1."
  skipSatisfyAscii isDigit
  word8 32
  status <- anyAsciiDecimalInt
  reason <- restOfLine
  found <- FB.many headerLine
  lineEnd
  pure (status, reason, found)
  where
    headerLine = do
      name <- byteStringOf (skipSome (skipSatisfyAscii tokenChar))
      word8 58
      value <- restOfLine
      pure (BS8.map lower name, value)
    restOfLine = do
      line <- byteStringOf (skipMany (notByte 10))
      word8 10
      pure (BS.dropWhile blank (dropWhileEnd blank line))
    lineEnd = skipMany (word8 13) *> word8 10
    blank byte = byte == 32 || byte == 9 || byte == 13
    tokenChar c = c > ' ' && c < '\DEL' && c /= ':'
    lower c = if c >= 'A' && c <= 'Z' then chr (fromEnum c + 32) else c

-- JSON, to the Value of aeson

jsonFlat :: FB.Parser () Value
jsonFlat = spaces *> value <* spaces
  where
    spaces = skipMany (skipSatisfyAscii (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t'))
    value =
      (String <$> string)
        <|> (word8 123 *> spaces *> (Object . JK.fromList <$> members) <* word8 125)
        <|> (word8 91 *> spaces *> (Array . V.fromList <$> elements) <* word8 93)
        <|> (Bool True <$ byteString "true")
        <|> (Bool False <$ byteString "false")
        <|> (Null <$ byteString "null")
        <|> number
    members = sepByComma ((,) <$> (fromText <$> string) <* spaces <* word8 58 <*> jsonFlat) <|> pure []
    elements = sepByComma jsonFlat <|> pure []
    sepByComma item = (:) <$> (item <* spaces) <*> FB.many (word8 44 *> spaces *> item <* spaces)
    string = do
      word8 34
      plain <- byteStringOf (skipMany plainByte)
      (word8 34 *> decoded plain) <|> (escaped [plain] >>= decoded . BS.concat . reverse)
    plainByte = anyWord8 >>= \byte -> if byte == 34 || byte == 92 || byte < 32 then failed else pure ()
    decoded bytes = either (const failed) pure (decodeUtf8' bytes)
    -- The rest of a string that holds escapes, its pieces in reverse.
    escaped pieces = do
      word8 92
      piece <- escape
      plain <- byteStringOf (skipMany plainByte)
      (reverse' (plain : piece : pieces) <$ word8 34) <|> escaped (plain : piece : pieces)
    reverse' = id
    escape =
      anyWord8 >>= \case
        34 -> pure "\""
        92 -> pure "\\"
        47 -> pure "/"
        98 -> pure "\b"
        102 -> pure "\f"
        110 -> pure "\n"
        114 -> pure "\r"
        116 -> pure "\t"
        117 -> do
          high <- hex4
          if high >= 0xD800 && high < 0xDC00
            then do
              word8 92 *> word8 117
              low <- hex4
              pure (encodeUtf8 (singleton (chr (0x10000 + (high - 0xD800) * 0x400 + (low - 0xDC00)))))
            else pure (encodeUtf8 (singleton (chr high)))
        _ -> failed
    hex4 = isolate 4 anyAsciiHexInt
    number = do
      negative <- (True <$ word8 45) <|> pure False
      whole <- anyAsciiDecimalInteger
      (fraction, digits) <- (word8 46 *> withByteString anyAsciiDecimalInteger (\n bytes -> pure (n, BS.length bytes))) <|> pure (0, 0)
      power <- ((word8 101 <|> word8 69) *> signedInt) <|> pure 0
      let mantissa = whole * 10 ^ digits + fraction
      pure (Number (scientific (if negative then negate mantissa else mantissa) (power - digits)))
    signedInt = (word8 45 *> (negate <$> anyAsciiDecimalInt)) <|> (word8 43 *> anyAsciiDecimalInt) <|> anyAsciiDecimalInt
