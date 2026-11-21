-- | Image requests, and fetching against a fake HTTP server on loopback.
module Roon.ImageSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (wait, withAsync)
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBytes
import Roon.Effect.WebSocket (ServerAddress (..))
import Roon.Error (RoonError (..))
import Roon.Image
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
      , testCase "fetches an image" $ do
          let jpeg = ByteString.pack (take 70_000 (cycle [255, 216, 255, 224, 0, 16]))
          (request, outcome) <- withServer (ok "image/jpeg" jpeg) $ \address ->
            fetch address "key" (fitWithin 300)
          ByteString.takeWhile (/= 13) request @?= "GET /api/image/key?scale=fit&width=300&height=300 HTTP/1.1"
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
          (_, tooLarge) <- withServer (ok "image/png" (ByteString.replicate 1001 7)) $ \address ->
            fetchWithin limits address
          tooLarge @?= Left (ImageFailed "the image is larger than 1000 bytes")
          (_, exact) <- withServer (ok "image/png" (ByteString.replicate 1000 7)) $ \address ->
            fetchWithin limits address
          fmap (ByteString.length . (.bytes)) exact @?= Right 1000
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
    fetchWithin limits address = runEff (runErrorNoCallStack @RoonError (fetchImageWithin limits address "key" originalImage))
    fetch address key options = runEff (runErrorNoCallStack @RoonError (fetchImage address key options))
    ok contentType content =
      [ "HTTP/1.1 200 OK\r\nContent-Type: " <> contentType <> "\r\nContent-Length: " <> Char8.pack (show (ByteString.length content)) <> "\r\n\r\n"
      , content
      ]

listen :: IO Socket.Socket
listen = do
  listener <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  Socket.setSocketOption listener Socket.ReuseAddr 1
  Socket.bind listener (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
  Socket.listen listener 1
  pure listener

addressOf :: Socket.Socket -> IO ServerAddress
addressOf listener = do
  port <- Socket.socketPort listener
  pure ServerAddress {host = "127.0.0.1", port = fromIntegral port}

-- | Run a server that answers one connection: it reads the request head,
-- sends the answer and closes. Returns the request with the result.
withServer :: [ByteString] -> (ServerAddress -> IO a) -> IO (ByteString, a)
withServer answer action =
  bracket listen Socket.close $ \listener -> do
    address <- addressOf listener
    withAsync (serve listener) $ \server -> do
      result <- action address
      request <- wait server
      pure (request, result)
  where
    serve listener =
      bracket (fst <$> Socket.accept listener) Socket.close $ \peer -> do
        request <- readHead peer ""
        mapM_ (SocketBytes.sendAll peer) answer
        pure request
    readHead peer seen
      | "\r\n\r\n" `ByteString.isInfixOf` seen = pure seen
      | otherwise = do
          more <- SocketBytes.recv peer 4096
          if ByteString.null more then pure seen else readHead peer (seen <> more)

-- | Run a server that accepts one connection and says nothing.
withSilentServer :: (ServerAddress -> IO a) -> IO a
withSilentServer action =
  bracket listen Socket.close $ \listener -> do
    address <- addressOf listener
    withAsync (bracket (fst <$> Socket.accept listener) Socket.close (\_ -> threadDelay 5_000_000)) $ \_ ->
      action address

-- | A port that was free a moment ago: bound, then closed.
closedPort :: IO Int
closedPort = bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \socket -> do
  Socket.bind socket (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
  fromIntegral <$> Socket.socketPort socket
