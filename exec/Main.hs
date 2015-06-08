import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.ByteString           (ByteString)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import           Util

type ClientMap = Map SockAddr (Chan ByteString)

-- | Creates a new socket to the client's address, and creates a Chan that's
-- continuously listened to on a new thread and passed along to the new socket
launchClientChannel :: MVar ClientMap -> SockAddr -> IO (Chan ByteString)
launchClientChannel clientsMVar clientSockAddr = do
  
  clientChannel <- newChan

  -- Start a new thread to 
  _ <- forkIO $ do
    -- Get the client's address and port number
    (hostName, serviceName) <- getSockAddrAddress clientSockAddr
    -- Create a new socket to talk to it on
    toClientSock <- socketToAddress hostName serviceName

    let displayName = "->" ++ hostName ++ ":" ++ serviceName
        showException e = putStrLn $ displayName ++ " " ++ show (e::SomeException)
        handleException = handle (\e -> showException e >> removeClient >> throwIO e)
        removeClient = do
          putStrLn $ displayName ++ " removing self..."
          close toClientSock
          withMVar clientsMVar $ return . Map.delete clientSockAddr

    handleException . forever $ do
      
      putStrLn $ displayName ++ " awaiting message..."
      
      message <- readChan clientChannel
      
      putStrLn $ displayName ++ " sending: " ++ (decode' message)
      _bytesSent <- send toClientSock message

      putStrLn $ displayName ++ " sent " ++ show _bytesSent ++ " bytes"
  return clientChannel

launchServer :: IO ()
launchServer = void . forkIO $ do

  serverSock <- createSocket serverPort

  -- TODO switch this to a broadcast channel and dup it in each client thread
  clientsMVar <- newMVar Map.empty

  forever $ do
    putStrLn . ("Clients now: " ++) . show . Map.keys =<< readMVar clientsMVar
    putStrLn $ "Server awaiting message..."

    -- Use recvFrom so we can see who the message came from
    (stuff, fromAddr) <- recvFrom serverSock 4096

    let got = decode' stuff :: String
    putStrLn $ "Server got: " ++ show got ++ " from " ++ show fromAddr

    -- Launch a thread to speak to the client if they're new,
    -- and broadcast the message to all clients
    modifyMVar_ clientsMVar $ \clients -> do
      -- Check if the client exists yet
      newClients <- case Map.lookup fromAddr clients of
        Just _ -> return clients
        Nothing            -> do
          clientChannel <- launchClientChannel clientsMVar fromAddr
          return $ Map.insert fromAddr clientChannel clients

      -- Broadcast the message to all clients
      forM_ newClients $ \clientChannel ->
        writeChan clientChannel stuff
      return newClients

launchClient :: IO ThreadId
launchClient = forkIO $ do

  -- Create a socket and 'bind' it 
  -- (rather than 'connect' it, as we want to receive
  -- from whatever port the server sends to us from)
  let hints = Just $ defaultHints { addrFlags = [AI_PASSIVE], addrFamily=AF_INET }
  -- Get a random port
  (addrInfo:_) <- getAddrInfo hints Nothing (Just "0")
  clientSock <- socket (addrFamily addrInfo) Datagram defaultProtocol
  bind clientSock (addrAddress addrInfo)

  clientPort <- socketPort clientSock
  let displayName = "127.0.0.1:" ++ show clientPort
  putStrLn $ "Launched client: " ++ displayName

  -- Get the address for the server's receive port

  (serverAddrInfo:_) <- getAddrInfo hints (Just serverName) (Just (show serverPort))

  -- Send a hello message to the server
  let message = "HELLO THERE FROM " ++ show clientPort ++ "!"
  _bytesSent <- sendBinaryTo clientSock (addrAddress serverAddrInfo) message

  -- Begin a receive loop for this client
  (`finally` close clientSock) . forever $ do
    -- (response, _) <- recvBinaryFrom clientSock
    response <- recvBinary clientSock
    putStrLn $ "<-" ++ displayName ++ " received: " ++ (response :: String)

main :: IO ()
main = do
  putStrLn "***Launching Server..."
  launchServer
  threadDelaySec 1
  putStrLn "***Launching client 1..."
  _client1 <- launchClient

  putStrLn "***Waiting 5 seconds..."
  threadDelaySec 5

  -- putStrLn "***Killing Client..."
  -- killThread _client1

  threadDelaySec 1
  putStrLn "***Launching client 2..."
  _client2 <- launchClient

  threadDelaySec 1
  putStrLn "***Done."

serverPort :: PortNumber
serverPort = 3000

serverName :: String
serverName = "127.0.0.1"

-- Connect a socket to a remote address
socketToAddress :: HostName -> ServiceName -> IO Socket
socketToAddress toAddress toPort = do

  let hints = Just $ defaultHints { addrFamily=AF_INET }

  (addrInfo:_) <- getAddrInfo hints (Just toAddress) (Just toPort)
  s <- socket (addrFamily addrInfo) Datagram defaultProtocol
  -- Connect once so we can use send rather than sendTo
  connect s (addrAddress addrInfo)
  return s

-- | Create a socket bound to our IP and the given port
createSocket :: PortNumber -> IO Socket
createSocket listenPort = do
  -- AI_PASSIVE means to use our current IP
  let hints = Just $ defaultHints { addrFlags = [AI_PASSIVE], addrFamily=AF_INET }
  -- Create a socket
  (addrInfo:_) <- getAddrInfo hints Nothing (Just (show listenPort))
  sock <- socket (addrFamily addrInfo) Datagram defaultProtocol
  -- Bind it to the complete address
  bind sock (addrAddress addrInfo)
  return sock


