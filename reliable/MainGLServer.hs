{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

import           Linear
import Control.Concurrent.STM
import qualified Data.Map               as Map
import           Control.Concurrent

import           Network.UDP.Pal
import           Control.Monad.State
import           Control.Lens
import Network.Socket

import Types
import Data.Time
main :: IO ()
main = do
  putStrLn "GL Server running."

  (broadcastChan, disconnectionsChan) <- createServer serverName serverPort packetSize
  ourAddr <- addrAddress <$> addressInfo (Just serverName) (Just (show serverPort))
  packetsFromClients <- atomically $ dupTChan broadcastChan
  let getPacketsFromClients = filter ((/= ourAddr) . fst) <$> liftIO (atomically (exhaustChan packetsFromClients))
      broadcastToClients    = liftIO . atomically . writeTChan broadcastChan . (ourAddr,)

  -- Launch the server's main thread to run physics simulations
  void . flip runStateT emptyAppState . forever $ do
    
    disconnections <- liftIO (atomically (exhaustChan disconnectionsChan))
    forM_ disconnections $ \fromAddr -> do
      liftIO $ putStrLn $ "GOODBYE: " ++ show fromAddr
      Just playerID <- use $ playerIDs . at fromAddr
      playerIDs . at fromAddr .= Nothing
      let message = DisconnectClient playerID
      interpretReliable message
      broadcastToClients (Reliable message)

    -- Process new packets
    packets <- getPacketsFromClients
    forM_ packets $ \(fromAddr, message) -> case message of
      Reliable   rel@(ConnectClient playerID _pose _color) -> do
        -- Associate the playerID with the fromAddr we already know,
        -- so we can send an accurate disconnect message later
        playerIDs . at fromAddr ?= playerID
        interpretReliable rel
        liftIO . putStrLn $ "Associated " ++ show fromAddr ++ " with userID " ++ playerID
      Reliable   rel -> interpretReliable rel
      Unreliable _clientPose -> do
        return ()

    -- Run physics
    cubePoses . traversed . posOrientation *= axisAngle (V3 0 1 0) 0.1
    cubes <- use $ cubePoses . to Map.toList
    let objPoses = map (\(cubeID, pose) -> ObjectPose cubeID pose) cubes

    now <- realToFrac . utctDayTime <$> liftIO getCurrentTime
    poses <- use playerPoses
    let plrPoses = map (\(playerID, pose) -> PlayerPose playerID pose)
          (Map.toList $ 
            poses &~ traversed . posPosition . _xy += V2 (sin now) (cos now)
            )
    --liftIO . putStrLn $ "Sending: " ++ show poses
    broadcastToClients (Unreliable (objPoses ++ plrPoses))

    liftIO $ threadDelay (1000000 `div` 60)
