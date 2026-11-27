module Roon.Moo.MessageSpec (tests) where

import Data.Aeson (Value, object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS (pack)
import Data.Map.Strict (empty, fromList, singleton)
import Data.Text (Text)
import Data.Text qualified as T (dropWhile, pack)
import Roon.Moo.Message (Body (..), MooMessage (..),
                         MooParseError (ContentTypeWithoutLength, InvalidContentLength, InvalidJson, MalformedHeader, MissingContentType, MissingHeaderEnd, MissingRequestId, TruncatedBody, UnexpectedBody, UnknownVerb),
                         RequestId (RequestId), Verb (..), encode, parse,
                         splitServiceMethod)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (Gen, arbitrary, chooseInt, counterexample,
                              elements, forAll, frequency, listOf, listOf1,
                              oneof, suchThat, testProperty, vectorOf, (===))

tests :: TestTree
tests =
  testGroup
    "Roon.Moo.Message"
    [ testGroup "encode" encodeTests
    , testGroup "parse" parseTests
    , testGroup "parse rejects" rejectTests
    , testProperty "parse . encode == Right" $
        forAll genMessage $ \msg ->
          counterexample (show (encode msg)) (parse (encode msg) === Right msg)
    , testCase "splitServiceMethod splits at the first slash" $ do
        splitServiceMethod "com.roonlabs.ping:1/ping" @?= Just ("com.roonlabs.ping:1", "ping")
        splitServiceMethod "service/a/b" @?= Just ("service", "a/b")
        splitServiceMethod "noslash" @?= Nothing
        splitServiceMethod "/method" @?= Nothing
    ]

encodeTests :: [TestTree]
encodeTests =
  [ testCase "request without body" $
      encode (message Request "com.roonlabs.registry:1/info" "0" NoBody)
        @?= "MOO/1 REQUEST com.roonlabs.registry:1/info\nRequest-Id: 0\n\n"
  , testCase "request with a JSON body" $
      encode (message Request "svc:1/method" "7" (JsonBody (object ["a" .= (1 :: Int)])))
        @?= "MOO/1 REQUEST svc:1/method\nRequest-Id: 7\nContent-Length: 7\nContent-Type: application/json\n\n{\"a\":1}"
  , testCase "reply with a raw body" $
      encode (message Complete "Success" "3" (RawBody "image/jpeg" "\255\216\255"))
        @?= "MOO/1 COMPLETE Success\nRequest-Id: 3\nContent-Length: 3\nContent-Type: image/jpeg\n\n\255\216\255"
  ]

parseTests :: [TestTree]
parseTests =
  [ testCase "reply without body" $
      parse "MOO/1 COMPLETE Success\nRequest-Id: 12\n\n"
        @?= Right (message Complete "Success" "12" NoBody)
  , testCase "reply with a JSON body" $
      parse "MOO/1 CONTINUE Registered\nRequest-Id: 1\nContent-Length: 17\nContent-Type: application/json\n\n{\"core_id\":\"abc\"}"
        @?= Right (message Continue "Registered" "1" (JsonBody (object ["core_id" .= ("abc" :: Text)])))
  , testCase "extra headers are kept, the three known ones are not" $
      parse "MOO/1 REQUEST com.roonlabs.ping:1/ping\nRequest-Id: 4\nLogging: quiet\n\n"
        @?= Right (message Request "com.roonlabs.ping:1/ping" "4" NoBody) {headers = singleton "Logging" "quiet"}
  , testCase "header values may omit the space after the colon" $
      parse "MOO/1 COMPLETE Success\nRequest-Id:5\n\n"
        @?= Right (message Complete "Success" "5" NoBody)
  , testCase "a zero Content-Length means no body" $
      parse "MOO/1 COMPLETE Success\nRequest-Id: 5\nContent-Length: 0\n\n"
        @?= Right (message Complete "Success" "5" NoBody)
  , testCase "bytes beyond Content-Length are ignored" $
      parse "MOO/1 COMPLETE Success\nRequest-Id: 5\nContent-Length: 2\nContent-Type: text/plain\n\nabcdef"
        @?= Right (message Complete "Success" "5" (RawBody "text/plain" "ab"))
  , testCase "a body may itself contain an empty line" $
      parse "MOO/1 COMPLETE Success\nRequest-Id: 5\nContent-Length: 4\nContent-Type: text/plain\n\na\n\nb"
        @?= Right (message Complete "Success" "5" (RawBody "text/plain" "a\n\nb"))
  ]

rejectTests :: [TestTree]
rejectTests =
  [ rejects "an empty frame" "" (== MissingHeaderEnd)
  , rejects "a frame without the empty line" "MOO/1 COMPLETE Success\nRequest-Id: 1\n" (== MissingHeaderEnd)
  , rejects "another protocol version" "MOO/2 COMPLETE Success\nRequest-Id: 1\n\n" isMalformedHeader
  , rejects "a header line without a colon" "MOO/1 COMPLETE Success\nRequest-Id 1\n\n" isMalformedHeader
  , rejects "an unknown verb" "MOO/1 FROBNICATE Success\nRequest-Id: 1\n\n" (== UnknownVerb "FROBNICATE")
  , rejects "a missing Request-Id" "MOO/1 COMPLETE Success\nLogging: quiet\n\n" (== MissingRequestId)
  , rejects
      "a Content-Type without a Content-Length"
      "MOO/1 COMPLETE Success\nRequest-Id: 1\nContent-Type: application/json\n\n"
      (== ContentTypeWithoutLength)
  , rejects
      "a body without a Content-Type"
      "MOO/1 COMPLETE Success\nRequest-Id: 1\nContent-Length: 2\n\n{}"
      (== MissingContentType)
  , rejects "an unannounced body" "MOO/1 COMPLETE Success\nRequest-Id: 1\n\n{}" (== UnexpectedBody)
  , rejects
      "a truncated body"
      "MOO/1 COMPLETE Success\nRequest-Id: 1\nContent-Length: 10\nContent-Type: text/plain\n\nabc"
      (== TruncatedBody 10 3)
  , rejects
      "a non-numeric Content-Length"
      "MOO/1 COMPLETE Success\nRequest-Id: 1\nContent-Length: two\nContent-Type: text/plain\n\nab"
      (== InvalidContentLength "two")
  , rejects
      "invalid JSON"
      "MOO/1 COMPLETE Success\nRequest-Id: 1\nContent-Length: 1\nContent-Type: application/json\n\n{"
      isInvalidJson
  ]
  where
    rejects label frame expected = testCase label $ case parse frame of
      Left err
        | expected err -> pure ()
        | otherwise -> assertFailure ("unexpected error: " <> show err)
      Right msg -> assertFailure ("unexpectedly parsed: " <> show msg)
    isMalformedHeader = \case
      MalformedHeader _ _ -> True
      _ -> False
    isInvalidJson = \case
      InvalidJson _ -> True
      _ -> False

message :: Verb -> Text -> Text -> Body -> MooMessage
message verb name rid body =
  MooMessage {verb, name, requestId = RequestId rid, headers = empty, body}

-- Generators. They only produce messages that the format can represent:
-- no line feed in a line, no leading space in a header value, a non-empty
-- raw body whose content type is not the JSON one.

genMessage :: Gen MooMessage
genMessage = do
  verb <- elements [minBound .. maxBound]
  name <- genLine
  requestId <- RequestId <$> genToken
  headers <- fromList <$> listOf ((,) <$> genHeaderName <*> genHeaderValue)
  body <- genBody
  pure MooMessage {verb, name, requestId, headers, body}

genBody :: Gen Body
genBody =
  frequency
    [ (1, pure NoBody)
    , (3, JsonBody <$> (arbitrary :: Gen Value))
    , (1, RawBody <$> elements ["image/jpeg", "image/png", "text/plain"] <*> genBytes)
    ]

genBytes :: Gen ByteString
genBytes = do
  size <- chooseInt (1, 64)
  BS.pack <$> vectorOf size arbitrary

-- | Any text that fits on a line.
genLine :: Gen Text
genLine = T.pack <$> listOf (genChar `suchThat` (/= '\n'))

genToken :: Gen Text
genToken = T.pack <$> listOf1 (elements (['a' .. 'z'] <> ['0' .. '9']))

genHeaderName :: Gen Text
genHeaderName = genName `suchThat` (`notElem` ["Request-Id", "Content-Length", "Content-Type"])
  where
    genName = T.pack <$> listOf1 (elements (['A' .. 'Z'] <> ['a' .. 'z'] <> "-"))

genHeaderValue :: Gen Text
genHeaderValue = T.dropWhile (== ' ') <$> genLine

genChar :: Gen Char
genChar = oneof [elements (['a' .. 'z'] <> " :/.-_{}\""), elements "éß→音", arbitrary `suchThat` (\c -> c < '\xD800' || c > '\xDFFF')]
