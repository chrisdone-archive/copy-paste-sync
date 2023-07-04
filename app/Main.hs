-- | Run as:
--
-- $ copy-paste-sync linux 3232 http://192.168.8.1:3000 $HOME/copy-paste-sync.pass
-- Listening on port 3232
-- Sending clipboard pushes to http://192.168.8.1:3000
--
-- 1. Starts waiting for incoming clipboard pushes.
-- 3. Sends outgoing clipboard pushes to $ip.

{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Network.Wai as Wai
import Control.Exception.Safe
import Network.HTTP.Types
import System.Process.Typed
import System.Environment
import Network.Wai.Handler.Warp (run)
import Control.Concurrent.Async
import Control.Concurrent
import Network.HTTP.Conduit as Http
import qualified Data.ByteString.Char8 as S8
import System.IO
import System.Timeout
import Data.IORef
import Data.ByteString.Lazy (ByteString)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    os:localPort:remoteUri:key:_ <- getArgs
    request <- parseUrlThrow remoteUri
    putStrLn $ "Listening on port " ++ localPort
    putStrLn $ "Sending events to " ++ remoteUri
    current <- newIORef mempty
    concurrently_
      (sender key os current request)
      (run (read localPort) (app key os current))

app :: FilePath -> String -> IORef ByteString -> Application
app key os current request respond = do
    encrypted <- strictRequestBody request
    payload <- readProcessStdout_ $ setStdin (byteStringInput encrypted) $ proc "openssl" ["enc","-base64","-d","-bf-cbc","-in","/dev/stdin","-out","/dev/stdout","-kfile",key]

    S8.putStr "Received new clipboard, setting ... "
    writeIORef current payload
    -- S8.putStr $ S8.pack $ show payload
    runProcess_ $ setStdin (byteStringInput payload) $
      case os of
        "linux" -> proc "xclip" ["-selection","clipboard"]
        "macos" -> proc "pbcopy" []
        _ -> error "unsupported OS."
    S8.putStrLn "done."
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Thanks."

sender :: FilePath -> String -> IORef ByteString -> Http.Request -> IO ()
sender key os current request = forever $ do
  threadDelay $ 1000 * 500
  previous <- readIORef current
  payload0 <- readProcessStdout_ $
    case os of
        "linux" -> proc "xclip" ["-selection","clipboard","-o","/dev/stdout"]
        "macos" -> proc "pbpaste" []
        _ -> error "unsupported OS."
  when (payload0 /= previous) $ void $ tryAny $ do
    writeIORef current payload0

    payload <- readProcessStdout_ $ setStdin (byteStringInput payload0) $ proc "openssl" ["enc","-base64","-e","-bf-cbc","-in","/dev/stdin","-out","/dev/stdout","-kfile",key]

    let request' =
          request { method = "PUT", Http.requestBody = RequestBodyLBS payload }
    S8.putStr "Pushing new clipboard ... "
    manager <- newManager $ tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro (1000 * 1000) }
    mresult <- timeout (1000 * 1000) $ httpLbs request' manager
    case mresult of
      Nothing -> S8.putStrLn "timed out."
      Just{} -> S8.putStrLn "done."
