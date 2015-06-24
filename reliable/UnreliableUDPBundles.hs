{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
module UnreliableUDPBundles where

import Data.ByteString (ByteString)
import Control.Lens
import Control.Monad
import           Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State
import Control.Concurrent.STM
import Control.Concurrent

import GHC.Generics
import Data.Binary
import Data.Monoid
import Data.Maybe
import Network.UDP.Pal.Binary
import Halive.Concurrent
-- Sending packet bundles

sendUnreliable :: TChan UnreliablePacket -> Int -> ByteString -> IO ()
sendUnreliable chan bundleNum payload = do
  
  let packet = UnreliablePacket bundleNum payload

  atomically $ writeTChan chan packet

-- Receiving packet bundles



-- App test


data AppState = AppState
  { _apsNextBundleNum :: Int
  }

makeLenses ''AppState

newAppState :: AppState
newAppState = AppState 0


incrBundleNum :: StateT AppState IO Int
incrBundleNum = apsNextBundleNum <<+= 1

main :: IO ()
main = do

  chan <- newTChanIO
  collection <- makeCollector (atomically (readTChan chan))
  void . flip runStateT newAppState $ do
    
    bundleNum <- incrBundleNum
    let bundle = Map.fromList [(x,y) | x <- [0..20], y <- [0..20]] :: Map Int Int
    forM_ (Map.toList bundle) $ \(x,y) -> 
      liftIO $ sendUnreliable chan bundleNum (encode' (x,y))
    
    -- Give the messages time to reach the collector
    liftIO $ threadDelay 100000

    -- Receive one frame of data
    results <- liftIO $ Map.fromList . map decode' <$> collectBundle collection bundleNum
    liftIO $ print (results :: Map Int Int)
