-- | Image requests, and fetching against a fake HTTP server on loopback.
module Roon.ImageSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (wait, withAsync)
import Control.Exception (bracket)
import Data.ByteString (ByteString, isInfixOf)
import Data.ByteString qualified as BS (length, null, pack, replicate,
                                        takeWhile)
import Data.ByteString.Char8 qualified as BS8 (pack)
import Data.Text qualified as T (replicate)
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.HTTP (Request (..), Response (..), runHttpWith)
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), Socket,
                       SocketOption (ReuseAddr), SocketType (Stream), accept,
                       bind, close, defaultProtocol, setSocketOption,
                       socketPort, tupleToHostAddress)
import Network.Socket qualified as N (listen, socket)
import Network.Socket.ByteString (recv, sendAll)
import Roon.Effect.WebSocket (ServerAddress (..))
import Roon.Error (RoonError (..))
import Roon.Image (Image (Image, bytes, contentType), ImageFormat (Jpeg, Png),
                   ImageLimits (maxBytes, microseconds),
                   ImageOptions (ImageOptions, format, size),
                   ImageSize (Original, Scaled), Scale (Fill, Stretch),
                   defaultImageLimits, fetchImage, fitWithin, imageTarget,
                   originalImage, runImageHttp)
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 10_000_000) $
    testGroup
      "Roon.Image"
      [ testCase "asks for the stored image without a query" $
          imageTarget "846a4ef9" originalImage @?= "/api/image/846a4ef9"
      , testCase "asks for a size and a format" $ do
          imageTarget "k" (fitWithin 300) @?= "/api/image/k?scale=fit&width=300&height=300"
          imageTarget "k" ImageOptions {size = Scaled Fill 640 480, format = Just Jpeg}
            @?= "/api/image/k?scale=fill&width=640&height=480&format=image/jpeg"
          imageTarget "k" ImageOptions {size = Scaled Stretch 1 2, format = Just Png}
            @?= "/api/image/k?scale=stretch&width=1&height=2&format=image/png"
          imageTarget "k" ImageOptions {size = Original, format = Just Png} @?= "/api/image/k?format=image/png"
      , testCase "encodes a key that would change the request" $
          imageTarget "a/b?c d\n\233" originalImage @?= "/api/image/a%2Fb%3Fc%20d%0A%C3%A9"
      , testCase "asks in clear for the target of the key, on the port of the server" $ do
          let echo scheme request = Response {status = 200, reason = "OK", headers = [("content-type", "image/png")], body = BS8.pack (show scheme <> " " <> request.host <> " " <> show request.port <> " ") <> request.method <> " " <> request.target}
          outcome <- answered echo (fetchImage ServerAddress {host = "192.0.2.7", port = 9330} "k" (fitWithin 64))
          outcome @?= Right Image {contentType = "image/png", bytes = "Http 192.0.2.7 9330 GET /api/image/k?scale=fit&width=64&height=64"}
      , testCase "a refusal answered from a function, with at most 1024 bytes of explanation" $ do
          let refuse _ _ = Response {status = 400, reason = "Bad Request", headers = [], body = BS.replicate 5000 120}
          outcome <- answered refuse (fetchImage ServerAddress {host = "192.0.2.7", port = 9330} "k" originalImage)
          outcome @?= Left (ImageFailed ("400 Bad Request " <> T.replicate 1024 "x"))
      , testCase "fetches an image" $ do
          let jpeg = BS.pack (take 70_000 (cycle [255, 216, 255, 224, 0, 16]))
          (request, outcome) <- withServer (ok "image/jpeg" jpeg) $ \address ->
            fetch address "key" (fitWithin 300)
          BS.takeWhile (/= 13) request @?= "GET /api/image/key?scale=fit&width=300&height=300 HTTP/1.1"
          outcome @?= Right Image {contentType = "image/jpeg", bytes = jpeg}
      , testCase "reports a refusal with the explanation of the server" $ do
          (_, unknown) <- withServer ["HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n"] $ \address ->
            fetch address "nope" originalImage
          unknown @?= Left (ImageFailed "404 Not Found")
          (_, invalid) <- withServer ["HTTP/1.1 400 Bad Request\r\nContent-Type: text/plain\r\nContent-Length: 20\r\n\r\nInvalid Scale: bogus"] $ \address ->
            fetch address "key" originalImage
          invalid @?= Left (ImageFailed "400 Bad Request Invalid Scale: bogus")
      , testCase "refuses an image larger than the limit, accepts one that reaches it" $ do
          let limits = defaultImageLimits {maxBytes = 1000}
          (_, tooLarge) <- withServer (ok "image/png" (BS.replicate 1001 7)) $ \address ->
            fetchWithin limits address
          tooLarge @?= Left (ImageFailed "the image is larger than 1000 bytes")
          (_, exact) <- withServer (ok "image/png" (BS.replicate 1000 7)) $ \address ->
            fetchWithin limits address
          fmap (BS.length . (.bytes)) exact @?= Right 1000
      , testCase "reports what is not HTTP, and a body cut short" $ do
          (_, notHttp) <- withServer ["MOO/1 COMPLETE Success\n\n"] $ \address -> fetch address "key" originalImage
          case notHttp of
            Left (ImageFailed _) -> pure ()
            other -> assertFailure ("expected ImageFailed, got " <> show other)
          (_, cut) <- withServer ["HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\nabc"] $ \address -> fetch address "key" originalImage
          cut @?= Left (ImageFailed "TruncatedBody 7")
      , testCase "times out on a server that does not answer" $ do
          let limits = defaultImageLimits {microseconds = 100_000}
          outcome <- withSilentServer $ \address ->
            fetchWithin limits address
          outcome @?= Left (TimedOut "image")
      , testCase "reports a refused connection" $ do
          port <- closedPort
          outcome <- fetch ServerAddress {host = "127.0.0.1", port} "key" originalImage
          case outcome of
            Left (ConnectFailed _) -> pure ()
            other -> assertFailure ("expected ConnectFailed, got " <> show other)
      ]
  where
    fetchWithin limits address = runEff (runErrorNoCallStack @RoonError (runImageHttp limits (fetchImage address "key" originalImage)))
    fetch address key options = runEff (runErrorNoCallStack @RoonError (runImageHttp defaultImageLimits (fetchImage address key options)))
    -- The effect answered from a function: what is asked, without a server.
    answered response = runEff . runErrorNoCallStack @RoonError . runHttpWith (\scheme request -> pure (response scheme request))
    ok contentType content =
      [ "HTTP/1.1 200 OK\r\nContent-Type: " <> contentType <> "\r\nContent-Length: " <> BS8.pack (show (BS.length content)) <> "\r\n\r\n"
      , content
      ]

listen :: IO Socket
listen = do
  listener <- N.socket AF_INET Stream defaultProtocol
  setSocketOption listener ReuseAddr 1
  bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  N.listen listener 1
  pure listener

addressOf :: Socket -> IO ServerAddress
addressOf listener = do
  port <- socketPort listener
  pure ServerAddress {host = "127.0.0.1", port = fromIntegral port}

-- | Run a server that answers one connection: it reads the request head,
-- sends the answer and closes. Returns the request with the result.
withServer :: [ByteString] -> (ServerAddress -> IO a) -> IO (ByteString, a)
withServer answer action =
  bracket listen close $ \listener -> do
    address <- addressOf listener
    withAsync (serve listener) $ \server -> do
      result <- action address
      request <- wait server
      pure (request, result)
  where
    serve listener =
      bracket (fst <$> accept listener) close $ \peer -> do
        request <- readHead peer ""
        mapM_ (sendAll peer) answer
        pure request
    readHead peer seen
      | "\r\n\r\n" `isInfixOf` seen = pure seen
      | otherwise = do
          more <- recv peer 4096
          if BS.null more then pure seen else readHead peer (seen <> more)

-- | Run a server that accepts one connection and says nothing.
withSilentServer :: (ServerAddress -> IO a) -> IO a
withSilentServer action =
  bracket listen close $ \listener -> do
    address <- addressOf listener
    withAsync (bracket (fst <$> accept listener) close (\_ -> threadDelay 5_000_000)) $ \_ ->
      action address

-- | A port that was free a moment ago: bound, then closed.
closedPort :: IO Int
closedPort = bracket (N.socket AF_INET Stream defaultProtocol) close $ \socket -> do
  bind socket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  fromIntegral <$> socketPort socket
