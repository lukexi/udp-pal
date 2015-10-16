import Network.UDP.Pal
import Network.Socket
import Control.Concurrent
import Control.Monad

magicNumber :: Int
magicNumber = 12345

main = do
  putStrLn "Broadcast example"
  privateIP <- findPrivateNetIP
  putStrLn $ "IP is: " ++ show privateIP

  socket <- socketWithDest "255.255.255.255" 3001 4096
  setSocketOption (bsSocket (swdBoundSocket socket)) Broadcast 1

  forkIO . forever $ do
    sendBinary socket magicNumber
    threadDelay (1000000 `div` 2)

  putStrLn "Waiting one sec..."
  threadDelay 1000000

  anyHost <- inet_ntoa iNADDR_ANY
  receiveSocket <- boundSocket (Just anyHost) aNY_PORT 4096


  forever $ do
    putStrLn "Receiving..."
    blah <- receiveFromDecoded receiveSocket
    print (blah :: (Int, SockAddr))



  putStrLn "Done!"
