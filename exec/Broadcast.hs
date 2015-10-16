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

main = do
  killThreads
  putStrLn "Broadcast example"
  privateIP <- findPrivateNetIP
  putStrLn $ "IP is: " ++ show privateIP
  
  socket <- broadcastSocket 9999 4096

  forkIO' . forever $ do
    putStrLn "Tick!"
    sendBinary socket magicNumber
    threadDelay (1000000 `div` 2)

  anyHost <- inet_ntoa iNADDR_ANY
  -- receiveSocket <- boundSocket (Just anyHost) aNY_PORT 4096

  receiveSocket <- boundSocket (Just anyHost) 9999 4096


  forever $ do
    putStrLn "Receiving..."
    blah <- receiveFromDecoded receiveSocket
    print (blah :: (Int, SockAddr))



  putStrLn "Done!"
