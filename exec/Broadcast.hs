import Network.UDP.Pal
import Network.Socket
import Control.Concurrent
import Control.Monad
import Halive.Utils
import Halive.Concurrent

magicNumber :: Int
magicNumber = 12345

broadcastPort = 9999
bufferSize = 4096

broadcastSocket port bufferSize = do
  socket <- socketWithDest broadcastIP 9999 4096
  setSocketOption (bsSocket (swdBoundSocket socket)) Broadcast 1
  return socket

broadcastIP = "255.255.255.255"

boundSocketAny port bufferSize = do
  anyHost <- inet_ntoa iNADDR_ANY
  boundSocket (Just anyHost) 9999 4096


beginSearch = forkIO' $ do

  -- Begin trying to receive a server beacon message
  receiveSocket <- boundSocketAny 9999 4096
  searchResultMVar <- newEmptyMVar
  searchThread <- forkIO . forever $ do
    putStrLn "Receiving..."
    receivedData <- receiveFromDecoded receiveSocket
    case receivedData of
      (receivedMagicNumber, SockAddrInet _port hostAddress) | receivedMagicNumber == magicNumber -> do
        hostAddressString <- inet_ntoa hostAddress
        putMVar searchResultMVar hostAddressString
      _ -> return ()

  -- Search for 1 second
  threadDelay 1000000
  killThread searchThread

  -- Check the results
  searchResult <- tryReadMVar searchResultMVar
  case searchResult of
    Just foundServerIP -> putStrLn $ "connectToServer: " ++ foundServerIP
    Nothing -> putStrLn "startServerAndBroadcaster"

beginBroadcaster = do
  socket <- broadcastSocket 9999 4096

  forkIO' . forever $ do
    putStrLn "Tick!"
    sendBinary socket magicNumber
    threadDelay (1000000 `div` 2)

main = do
  killThreads
  putStrLn "Broadcast example"
  privateIP <- findPrivateNetIP
  putStrLn $ "IP is: " ++ show privateIP

  beginBroadcaster
  beginSearch

  threadDelay 2000000
  putStrLn "Done!"
