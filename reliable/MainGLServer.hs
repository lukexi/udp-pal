{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

import           Linear
import qualified Data.Map               as Map
import           Control.Concurrent
import           Control.Concurrent.STM

import           Network.UDP.Pal
import           Control.Monad.State
import           Control.Lens
import           Data.Time

import           Types


main :: IO ()
main = do
  putStrLn "GL Server running."
  
  (getPacketsFromClients, broadcastToClients, disconnectionsChan) <- createServer serverName serverPort packetSize
  
  -- The server's main thread then runs physics simulations
  void . flip runStateT emptyAppState . forever $ do
    
    disconnections <- liftIO (atomically (exhaustChan disconnectionsChan))
    forM_ disconnections $ \fromAddr -> do
      -- For each SockAddr we've detected a disconnection from,
      -- find its associated player ID and broadcast a message to clients
      -- informing them that the player has left so they can clear any
      -- visible rendering of that player.

      Just playerID <- use $ playerIDs . at fromAddr
      playerIDs . at fromAddr .= Nothing
      let message = DisconnectClient playerID
      interpretToState message
      broadcastToClients (Reliable message)

      liftIO $ putStrLn $ "Goodbye: " ++ show fromAddr

    -- Process new packets
    interpredNetworkPacketsFromOthers getPacketsFromClients $ \fromAddr msg -> case msg of
      message@(ConnectClient playerID _pose _color) -> do
        -- Associate the playerID with the fromAddr we already know,
        -- so we can send an accurate disconnect message later
        playerIDs . at fromAddr ?= playerID
        interpretToState message
      message -> interpretToState message

    -- Run physics
    cubePoses . traversed . posOrientation *= axisAngle (V3 0 1 0) 0.1
    cubes <- use $ cubePoses . to Map.toList
    let objPoses = map (uncurry ObjectPose) cubes

    now <- realToFrac . utctDayTime <$> liftIO getCurrentTime
    poses <- use playerPoses
    let plrPoses = map (uncurry PlayerPose)
          (Map.toList $ 
            poses &~ traversed . posPosition . _xy += V2 (sin now) (cos now)
            )
    --liftIO . putStrLn $ "Sending: " ++ show poses
    broadcastToClients (Unreliable (objPoses ++ plrPoses))

    liftIO $ threadDelay (1000000 `div` 60)
