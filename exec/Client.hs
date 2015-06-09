import Control.Concurrent
import Control.Exception
import Control.Monad

import Network.UDP.Pal
import Shared

main :: IO ()
main = do
  client <- makeClient serverName serverPort 4096

  displayName <- show <$> getSocketName (clientSocket client)
  putStrLn $ "*** Launched client: " ++ displayName


  forkIO . forever $ do
    -- Send a hello message to the server
    let message = "HELLO THERE FROM " ++ displayName ++ "!"
    _bytesSent <- sendEncoded client message
    threadDelaySec 0.5


  -- Begin a receive loop for this client
  (`finally` close (clientSocket client)) . forever $ do
    -- (response, _) <- recvBinaryFrom clientSock
    (response, _) <- receiveFromDecoded client
    putStrLn $ "<-" ++ displayName ++ " received: " ++ (response :: String)
