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
        createTransceiver "Server" (Right toClientSock)



  finallyClose . void . forever $ do
    -- Receive a message along with the address it originated from
    (newMessage, fromAddr) <- receiveFromRaw incomingSocket

    transceiver <- findClient fromAddr

    let packet = decode' newMessage :: WirePacket ObjectPose ObjectOp
    atomically $ writeTChan (tcIncomingRawPackets transceiver) packet

    liftIO . putStrLn $ "Received from: " ++ show fromAddr
                        ++ ": " ++ show packet


    print =<< atomically (exhaustChan (tcVerifiedPackets transceiver))
    -- atomically $ writeTChan outgoingPackets (Reliable (NameObject 42 "Oh hey!"))

main :: IO ()
main = do
  killThreads
  _            <- launchServer

  toServerSock <- socketWithDest serverName serverPort packetSize
  displayName  <- show <$> getSocketName (bsSocket (swdBoundSocket toServerSock))
  putStrLn $ "*** Launched client: " ++ displayName

  transceiver <- createTransceiver "Client" (Left toServerSock)

  -- Stream received packets into the Transceiver's packetsIn channel
  _receiveThread <- streamInto (tcIncomingRawPackets transceiver) 
    (fst <$> receiveFromDecoded (swdBoundSocket toServerSock) :: IO (WirePacket ObjectPose ObjectOp))

  forever $ do

    liftIO . atomically $ writeTChan (tcOutgoingPackets transceiver) (Reliable (ConnectClient "hello"))
    liftIO $ print =<< atomically (exhaustChan (tcVerifiedPackets transceiver))

    liftIO . atomically $ writeTChan (tcOutgoingPackets transceiver) (Reliable (ConnectClient "sailor"))
    liftIO $ print =<< atomically (exhaustChan (tcVerifiedPackets transceiver))

    liftIO . atomically $ writeTChan (tcOutgoingPackets transceiver) (Reliable (ConnectClient "!!!"))
    liftIO $ print =<< atomically (exhaustChan (tcVerifiedPackets transceiver))

    liftIO $ threadDelay 1000000
