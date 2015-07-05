-- {-# LANGUAGE DeriveAnyClass            #-}
-- {-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- import Data.Binary
-- import GHC.Generics

import           Control.Monad.State
import           Network.UDP.Pal

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import qualified Data.Map               as Map
import           Halive.Concurrent
import Types


serverPort :: PortNumber
serverPort = 3000

serverName :: String
serverName = "127.0.0.1"

packetSize :: Int
packetSize = 4096

launchServer :: IO ThreadId
launchServer = forkIO' $ do
  incomingSocket <- boundSocket (Just serverName) serverPort packetSize
  let finallyClose = flip finally (close (bsSocket incomingSocket))

  clients <- newMVar mempty
  let findClient fromAddr =
        modifyMVar clients $ \currentClients ->
          case Map.lookup fromAddr currentClients of
            Just client -> return (currentClients, client)
            Nothing     -> do
              client <- newServerToClientThread fromAddr
              return (Map.insert fromAddr client currentClients, client)
      newServerToClientThread fromAddr = do
        toClientSock <- connectedSocketToAddr fromAddr
        createReceiver "Server" (Right toClientSock)



  finallyClose . void . forever $ do
    -- Receive a message along with the address it originated from
    (newMessage, fromAddr) <- receiveFromRaw incomingSocket

    (incomingRawPackets, verifiedPackets, _outgoingPackets) <- findClient fromAddr

    let packet = decode' newMessage :: Packet ObjectPose ObjectOp
    atomically $ writeTChan incomingRawPackets packet

    liftIO . putStrLn $ "Received from: " ++ show fromAddr
                        ++ ": " ++ show packet


    print =<< atomically (exhaustChan verifiedPackets)
    -- atomically $ writeTChan outgoingPackets (Reliable (NameObject 42 "Oh hey!"))

main :: IO ()
main = do
  killThreads
  _            <- launchServer

  toServerSock <- socketWithDest serverName serverPort packetSize
  displayName  <- show <$> getSocketName (bsSocket (swdBoundSocket toServerSock))
  putStrLn $ "*** Launched client: " ++ displayName

  (incomingRawPackets, verifiedPackets, outgoingPackets) <- createReceiver "Client" (Left toServerSock)

  -- Stream received packets into the Receiver's packetsIn channel
  streamInto incomingRawPackets (fst <$> receiveFromDecoded (swdBoundSocket toServerSock) :: IO (Packet ObjectPose ObjectOp))

  forever $ do

    liftIO . atomically $ writeTChan outgoingPackets (Reliable (NameObject 0 "hello"))
    liftIO $ print =<< atomically (exhaustChan verifiedPackets)

    liftIO . atomically $ writeTChan outgoingPackets (Reliable (NameObject 1 "sailor"))
    liftIO $ print =<< atomically (exhaustChan verifiedPackets)

    liftIO . atomically $ writeTChan outgoingPackets (Reliable (NameObject 2 "!!!"))
    liftIO $ print =<< atomically (exhaustChan verifiedPackets)

    liftIO $ threadDelay 1000000
