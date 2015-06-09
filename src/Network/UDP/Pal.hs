module Network.UDP.Pal where
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.ByteString           (ByteString)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import           Network.UDP.Pal.Util

serverPort :: PortNumber
serverPort = 3000

serverName :: String
serverName = "127.0.0.1"

type ClientMap = Map SockAddr (Chan ByteString)

-- | Creates a new socket to the client's address, and creates a Chan that's
-- continuously listened to on a new thread and passed along to the new socket
launchClientThread :: MVar ClientMap -> SockAddr -> IO (Chan ByteString)
launchClientThread clientsMVar clientSockAddr = do
  
  clientChannel <- newChan

  -- Start a new thread to broadcast messages to the client
  _ <- forkIO $ do
    -- Get the client's address and port number
    (hostName, serviceName) <- getSockAddrAddress clientSockAddr
    -- Create a new socket to talk to it on
    toClientSock            <- connectedSocket hostName serviceName

    -- Have the thread close the socket and remove the client
    -- from the broadcast queue when an exception occurs
    let displayName = "->" ++ hostName ++ ":" ++ serviceName
        finisher e = do
          putStrLn $ displayName ++ " removing self due to " ++ show (e::SomeException)
          close toClientSock
          withMVar clientsMVar $ return . Map.delete clientSockAddr
          throwIO e

    handle finisher . forever $ do
      
      -- putStrLn $ displayName ++ " awaiting message..."
      
      message <- readChan clientChannel
      
      -- putStrLn $ displayName ++ " sending: " ++ (decode' message)
      _bytesSent <- send toClientSock message

      -- putStrLn $ displayName ++ " sent " ++ show _bytesSent ++ " bytes"
      return ()
  return clientChannel

launchServer :: IO ()
launchServer = void . forkIO $ do

  serverSock <- createSocket serverPort

  clientsMVar <- newMVar Map.empty

  forever $ do
    -- putStrLn . ("Clients now: " ++) . show . Map.keys =<< readMVar clientsMVar
    -- putStrLn $ "Server awaiting message..."

    -- Use recvFrom so we can see who the message came from
    (newMessage, fromAddr) <- recvFrom serverSock 4096

    -- let got = decode' newMessage :: String
    -- putStrLn $ "Server got: " ++ show got ++ " from " ++ show fromAddr

    -- Launch a thread to speak to the client if they're new,
    -- and broadcast the message to all clients
    modifyMVar_ clientsMVar $ \clients -> do
      -- Check if the client exists yet
      newClients <- case Map.lookup fromAddr clients of
        Just _ -> return clients
        Nothing            -> do
          clientChannel <- launchClientThread clientsMVar fromAddr
          return $ Map.insert fromAddr clientChannel clients

      -- Broadcast the message to all clients
      forM_ newClients $ \clientChannel ->
        writeChan clientChannel newMessage
      return newClients

launchClient :: IO ThreadId
launchClient = forkIO $ do

  -- We don't 'connect' as we want to receive from whatever address
  -- the server decides to use to talk to us.
  -- 0 gets a random port to receive from
  clientSock <- createSocket 0

  clientPort   <- socketPort clientSock
  let displayName = "127.0.0.1:" ++ show clientPort
  putStrLn $ "Launched client: " ++ displayName

  -- Get the address for the server's receive port

  serverAddrInfo <- addressInfo (Just serverName) (Just (show serverPort))

  -- Send a hello message to the server
  let message = "HELLO THERE FROM " ++ show clientPort ++ "!"
  _bytesSent <- sendBinaryTo clientSock (addrAddress serverAddrInfo) message

  -- Begin a receive loop for this client
  (`finally` close clientSock) . forever $ do
    -- (response, _) <- recvBinaryFrom clientSock
    response <- recvBinary clientSock
    putStrLn $ "<-" ++ displayName ++ " received: " ++ (response :: String)


-- Connect a socket connected to a remote address
-- so 'send' and 'recv' work
connectedSocket :: HostName -> ServiceName -> IO Socket
connectedSocket toAddress toPort = do
  addrInfo <- addressInfo (Just toAddress) (Just toPort)
  s <- socket (addrFamily addrInfo) Datagram defaultProtocol
  -- Connect once so we can use send rather than sendTo
  connect s (addrAddress addrInfo)
  return s



addressInfo :: Maybe HostName -> Maybe ServiceName -> IO AddrInfo
-- getAddrInfo always returns a non-empty list, or throws an exception
addressInfo address port = head <$> getAddrInfo hints address port
  where 
    -- AI_PASSIVE means to get our current IP if none provided
    hints = Just $ defaultHints { addrFlags = [AI_PASSIVE], addrFamily=AF_INET }

-- | Create a socket bound to our IP and the given port
createSocket :: PortNumber -> IO Socket
createSocket listenPort = do
  -- Create a socket
  addrInfo <- addressInfo Nothing (Just (show listenPort))
  sock <- socket (addrFamily addrInfo) Datagram defaultProtocol
  -- Bind it to the complete address
  bind sock (addrAddress addrInfo)
  return sock


