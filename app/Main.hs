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

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    localPort:remoteUri:_ <- getArgs
    request <- parseUrlThrow remoteUri
    putStrLn $ "Listening on port " ++ localPort
    putStrLn $ "Sending events to " ++ remoteUri
    concurrently_
      (sender request)
      (run (read localPort) app)

app :: Application
app request respond = do
    S8.putStrLn "Received new clipboard."
    payload <- strictRequestBody request
    S8.putStr "Received new clipboard, setting ..."
    runProcess_ $ setStdin (byteStringInput payload) $ proc "xclip" ["-selection","clipboard"]
    S8.putStrLn "done."
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Thanks."

sender :: Http.Request -> IO ()
sender request = go "" where
 go previous = do
  threadDelay $ 1000 * 1000 * 1
  payload <- readProcessStdout_ $ proc "xclip" ["-selection","clipboard","-o","/dev/stdout"]
  when (payload /= previous) $ void $ tryAny $ do
    let request' =
          request { method = "PUT", Http.requestBody = RequestBodyLBS "<payload>" }
    S8.putStr "Pushing new clipboard ..."
    manager <- newManager tlsManagerSettings
    _ <- httpLbs request' manager
    S8.putStrLn "done."

{-
while true; do; socat -u TCP-LISTEN:8002,keepalive,reuseaddr,rcvbuf=131071 STDOUT; done
-}
