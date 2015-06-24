{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
import ReliableUDP
import Network.UDP.Pal
import Control.Monad.State
import Types

import Control.Exception
import Halive.Concurrent
import Control.Concurrent

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
      conn = newConnection :: Connection String String

  finallyClose . void . flip runStateT conn . forever $ do
    -- Receive a message along with the address it originated from
    (newMessage, fromAddr) <- receiveFromRaw incomingSocket
    toClientSock <- connectedSocketToAddr fromAddr

    let packet = decode' newMessage :: Packet String String
    liftIO . putStrLn $ "Received from: " ++ show fromAddr 
      ++ ": " ++ show packet

    case packet of
      UnreliablePacket bundleNum _payload -> do
        liftIO $ print "unreliable"
      ReliablePacket seqNum _payload -> do
        liftIO . putStrLn $ "Acknowledging: " ++ show seqNum
        _ <- sendBinaryConn toClientSock (ReliablePacketAck seqNum :: Packet String String)
        return ()
      ReliablePacketAck seqNum -> 
        receiveAck seqNum



main :: IO ()
main = do
  killThreads
  _ <- launchServer

  client <- socketWithDest serverName serverPort packetSize
  displayName <- show <$> getSocketName (bsSocket (swdBoundSocket client))
  putStrLn $ "*** Launched client: " ++ displayName

  let receivePacket = do
        (ack, _) <- receiveFromDecoded (swdBoundSocket client)
        let _ = ack :: Packet String String
        case ack of
          ReliablePacket seqNum _payload -> do
            _ <- sendEncoded client (ReliablePacketAck seqNum :: Packet String String)
            return ()
          UnreliablePacket _ _ -> liftIO $ print "unreliable"
          ReliablePacketAck seqNum -> 
            receiveAck seqNum

  let conn = newConnection :: Connection String String
  void . flip runStateT conn $ do
    
    sendReliable client "hello"

    receivePacket
    
    sendReliable client "sailor"

    receivePacket
    
    sendReliable client "!!!"

    receivePacket