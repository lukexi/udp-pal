import Control.Concurrent
import Control.Exception
import Control.Monad

import Network.UDP.Pal
import Halive.Concurrent

serverPort :: PortNumber
serverPort = 3000

serverName :: String
serverName = "127.0.0.1"

launchClient :: IO ThreadId
launchClient = forkIO' $ do

  client <- socketWithDest serverName serverPort 4096

  displayName <- show <$> getSocketName (bsSocket (swdBoundSocket client))
  putStrLn $ "*** Launched client: " ++ displayName

  -- Send a hello message to the server
  let message = "HELLO THERE FROM " ++ displayName ++ "!"
  _bytesSent <- sendEncoded client message

  -- Begin a receive loop for this client
  flip finally (close (bsSocket (swdBoundSocket client))) . forever $ do
    -- (response, _) <- recvBinaryFrom clientSock
    (response, _) <- receiveFromDecoded (swdBoundSocket client)
    putStrLn $ "<-" ++ displayName ++ " received: " ++ (response :: String)

main :: IO ()
main = do
  putStrLn "*** Launching Server..."
  echoServer serverName serverPort 4096
  threadDelaySec 1
  putStrLn "*** Launching client 1..."
  _client1 <- launchClient

  putStrLn "*** Waiting a second..."
  threadDelaySec 1

  -- putStrLn "*** Killing Client..."
  -- killThread _client1

  threadDelaySec 1
  putStrLn "*** Launching client 2..."
  _client2 <- launchClient

  threadDelaySec 1
  putStrLn "*** Done."
