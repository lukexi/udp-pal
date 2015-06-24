import Control.Concurrent
import Control.Exception
import Control.Monad

import Network.UDP.Pal
import Shared
import Halive.Concurrent

main :: IO ()
main = do
  socket <- socketWithDest serverName serverPort 4096

  displayName <- show <$> getSocketName (bsSocket (swdBoundSocket socket))
  putStrLn $ "*** Launched client: " ++ displayName


  _ <- forkIO' . forever $ do
    -- Send a hello message to the server
    let message = "HELLO THERE FROM " ++ displayName ++ "!"
    _bytesSent <- sendEncoded socket message
    threadDelaySec 0.5


  -- Begin a receive loop for this client
  flip finally (close (bsSocket (swdBoundSocket socket))) . forever $ do
    -- (response, _) <- recvBinaryFrom clientSock
    (response, _) <- receiveFromDecoded (swdBoundSocket socket)
    putStrLn $ "<-" ++ displayName ++ " received: " ++ (response :: String)
