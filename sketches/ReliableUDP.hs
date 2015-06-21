{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TemplateHaskell #-}
import Network.Socket
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import GHC.Generics
import Data.Binary
import Control.Monad.State
import Control.Lens
import Control.Concurrent
import           Data.Map (Map)
import qualified Data.Map as Map

data ReliablePacket = ReliablePacket 
  { rpkSeqNum  :: Int
  , rpkPayload :: ByteString 
  } deriving (Show, Generic, Binary)

data Connection = Connection
  { _connNextSeqNum :: Int
  , _connUnacked    :: Map Int ReliablePacket
  }
makeLenses ''Connection

newConnection :: Connection
newConnection = Connection 0 mempty

sendReliable payload = do
  seqNum <- connNextSeqNum <<+= 1
  let packet = ReliablePacket seqNum payload
  connUnacked . at seqNum ?= packet

  unacked <- use connUnacked

  forM_ unacked $ \aPacket -> do
    liftIO . putStrLn $ "Sending " ++ show unacked
    liftIO $ putStrLn ""

-- | Discard all packets less than the acknowledged sequence number
receiveAck ack = do
  unacked <- use connUnacked
  let (_smaller, larger) = Map.split ack unacked
  connUnacked .= larger

encode' = toStrict . encode

main = do

  flip runStateT newConnection $ do
    
    sendReliable (encode' "hello"  )
    receiveAck 0
    sendReliable (encode' "sailor" )
    receiveAck 1
    sendReliable (encode' "!!!"    )
