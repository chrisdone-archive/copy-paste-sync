-- | Run as:
--
-- $ copy-paste-sync 3232 http://192.168.8.1:3000
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

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    os:localPort:remoteUri:_ <- getArgs
    request <- parseUrlThrow remoteUri
    putStrLn $ "Listening on port " ++ localPort
    putStrLn $ "Sending events to " ++ remoteUri
    concurrently_
      (sender os request)
      (run (read localPort) (app os))

app :: String -> Application
app os request respond = do
    S8.putStrLn "Received new clipboard."
    payload <- strictRequestBody request
    S8.putStr "Received new clipboard, setting ..."
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

sender :: String -> Http.Request -> IO ()
sender os request = go "" where
 go previous = do
  threadDelay $ 1000 * 1000
  payload <- readProcessStdout_ $
    case os of
        "linux" -> proc "xclip" ["-selection","clipboard","-o","/dev/stdout"]
        "macos" -> proc "pbpaste" []
        _ -> error "unsupported OS."
  when (payload /= previous) $ void $ tryAny $ do
    let request' =
          request { method = "PUT", Http.requestBody = RequestBodyLBS "<payload>" }
    S8.putStr "Pushing new clipboard ..."
    manager <- newManager $ tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro (1000 * 1000) }
    mresult <- timeout (1000 * 1000) $ httpLbs request' manager
    case mresult of
      Nothing -> S8.putStrLn " timed out."
      Just{} -> S8.putStrLn "done."
  go payload
