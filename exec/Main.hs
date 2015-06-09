import Network.UDP.Pal
import Control.Concurrent
import Network.Socket (close, getSocketName, PortNumber)
import Control.Exception
import Control.Monad

serverPort :: PortNumber
serverPort = 3000

serverName :: String
serverName = "127.0.0.1"

launchClient :: IO ThreadId
launchClient = forkIO $ do

  client <- makeClient serverName serverPort

  displayName <- show <$> getSocketName (clientSocket client)
  putStrLn $ "*** Launched client: " ++ displayName

  -- Send a hello message to the server
  let message = "HELLO THERE FROM " ++ displayName ++ "!"
  _bytesSent <- sendEncoded client message

  -- Begin a receive loop for this client
  (`finally` close (clientSocket client)) . forever $ do
    -- (response, _) <- recvBinaryFrom clientSock
    response <- receiveDecoded client
    putStrLn $ "<-" ++ displayName ++ " received: " ++ (response :: String)

main :: IO ()
main = do
  putStrLn "*** Launching Server..."
  makeServer serverName serverPort
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


-- wishfulS = do
--   (broadcastChan, receiver) <- makeServer

--   receiver $ \message -> do 
--     echoToAll message
--     updateMyState message

--   results <- doSomeStuff
--   forM results (writeChan broadcastChan)
