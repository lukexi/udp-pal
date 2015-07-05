{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE RecordWildCards           #-}

-- import Data.Binary
-- import GHC.Generics

import           Control.Monad.State
import           Network.UDP.Pal

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import qualified Data.Map               as Map
import           Data.Map               (Map)
import           Halive.Concurrent
import           Control.Lens
import           Graphics.UI.GLFW.Pal
import           Graphics.GL.Pal2
import           Graphics.GL
import           Linear
import GHC.Generics
import Data.Binary
import Data.Maybe

data Pose = Pose
  { _posPosition    :: V3 GLfloat
  , _posOrientation :: Quaternion GLfloat 
  } deriving (Generic, Binary, Show)
makeLenses ''Pose

{-
SERVER LOGIC:
Reliable messages from clients (like 'CreateCube') should be broadcasted  (with a new seqNum) to all except the sender
Unreliable messages from clients (like 'ObjectPose') should be broadcasted to all except the sender
Server unreliable sim messages should be broadcasted to all clients
  clients should be ready for updates to non-existent objects and ignore them

Let's try starting with having the server expire cubes since it has to anyway.
Can add having the client expire them too (locally, no network message) later.

Try making the above configurable for the server setup we've got 
(i.e. create a server configured to handle messages in the above way)

-}

{-
Client sends hand and head updates unreliably to the server.
Server receives all these and broadcasts every frame to the all clients.
Client sends reliable "create cube" updates to the server.
Server re-broadcasts these to clients, and also starts physics sim
to send unreliable updates and expiration timer to send reliable "DeleteCube" message.

Server is running
  message accept loop
  send-threads for each client
  physics sim loop
    receives from clients
    simulate
    broadcasts to clients

Client is running
  message accept loop
  render loop
    receive messages
    receive input
    simulate
    render
    send updates to server

-}

serverPort :: PortNumber
serverPort = 3000

serverName :: String
serverName = "127.0.0.1"

packetSize :: Int
packetSize = 4096

resX, resY :: Int
(resX, resY) = (1024, 768)

launchServer :: IO ThreadId
launchServer = forkIO' $ do
  incomingSocket <- boundSocket (Just serverName) serverPort packetSize
  let finallyClose = flip finally (close (bsSocket incomingSocket))

  broadcastChan <- newBroadcastTChanIO

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

        broadcastsToClient <- atomically $ dupTChan broadcastChan
        (incomingRawPackets, verifiedPackets, outgoingPackets) <- createReceiver "Server" (Right toClientSock)
        threadID1 <- streamInto broadcastChan (atomically . readTChan $ verifiedPackets)
        threadID2 <- streamInto outgoingPackets (atomically . readTChan $ broadcastsToClient)
        return incomingRawPackets 

  finallyClose . void . forever $ do
    -- Receive a message along with the address it originated from
    (newMessage, fromAddr) <- receiveFromRaw incomingSocket

    -- Find the client receiver who should process this message
    incomingRawPackets <- findClient fromAddr

    -- Pass the decoded packet into the client's Receiver's incomingRawPackets channel
    let packet = decode' newMessage :: Packet ObjectPose ObjectOp
    atomically $ writeTChan incomingRawPackets packet

main :: IO ()
main = do
  killThreads
  _ <- launchServer

  toServerSock <- socketWithDest serverName serverPort packetSize
  displayName  <- show <$> getSocketName (bsSocket (swdBoundSocket toServerSock))
  putStrLn $ "*** Launched client: " ++ displayName

  (incomingRawPackets, verifiedPackets, outgoingPackets) <- createReceiver "Client" (Left toServerSock)

  -- Stream received packets into the Receiver's packetsIn channel
  streamInto incomingRawPackets (fst <$> receiveFromDecoded (swdBoundSocket toServerSock) :: IO (Packet ObjectPose ObjectOp))

  (window, events) <- createWindow "ReliableCubes" resX resY

  cubeProg   <- createShaderProgram "reliable/cube.vert" "reliable/cube.frag"
  cubeGeo    <- cubeGeometry ( 0.5 :: V3 GLfloat ) ( V3 1 1 1 )
  cube       <- entity cubeGeo  cubeProg 

  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  glEnable GL_CULL_FACE
  glCullFace GL_BACK

  useProgram (program cube)

  let newWorld = Map.singleton 0 $ 
        Pose { _posPosition = 0, _posOrientation = axisAngle (V3 0 1 0) 1 }
  forever . flip runStateT newWorld $ do
    processEvents events $ \e -> return ()

    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  
    projection  <- makeProjection window
    let playerPos = V3 0 0 5
        playerOrient = axisAngle (V3 0 1 0) 0
        view = viewMatrix playerPos playerOrient
        cam = uCamera ( uniforms cube )
        projectionView = projection !*! view
        eyePos = fromMaybe view (inv44 view) ^. translation
    glUniform3f ( unUniformLocation  cam )
        ( eyePos ^. _x )
        ( eyePos ^. _y )
        ( eyePos ^. _z )

    newCubes <- use id
    withVAO ( vAO cube ) $ 
      forM_ ( zip [0..] ( Map.elems newCubes ) ) $ \( i , obj ) -> do

        let model = mkTransformation (obj ^. posOrientation) (obj ^. posPosition)

        drawEntity model projectionView i cube

    swapBuffers window

    --liftIO . atomically $ writeTChan outgoingPackets (Reliable (NameObject 0 "hello"))
    --liftIO $ print =<< atomically (exhaustChan verifiedPackets)

    --liftIO . atomically $ writeTChan outgoingPackets (Reliable (NameObject 1 "sailor"))
    --liftIO $ print =<< atomically (exhaustChan verifiedPackets)

    --liftIO . atomically $ writeTChan outgoingPackets (Reliable (NameObject 2 "!!!"))
    --liftIO $ print =<< atomically (exhaustChan verifiedPackets)





-- | Get a view matrix for a camera at a given position and orientation
viewMatrix :: (RealFloat a, Conjugate a) => V3 a -> Quaternion a -> M44 a
viewMatrix position orientation = mkTransformation q (rotate q . negate $ position)
    where q = conjugate orientation



-- | Use the aspect ratio from the window to get a proper projection
makeProjection :: (Floating a, MonadIO m) => Window -> m (M44 a)
makeProjection win = do
    (w,h) <- getWindowSize win
    return $ perspective 45 (fromIntegral w / fromIntegral h) 0.01 100

drawEntity :: MonadIO m => M44 GLfloat -> M44 GLfloat -> GLfloat -> Entity -> m ()
drawEntity model projectionView drawID anEntity = do 

  let Uniforms{..} = uniforms anEntity

  uniformM44 uMVP ( projectionView !*! model)
  uniformM44 uInverseModel (fromMaybe model (inv44 model))
  uniformM44 uModel model

  let dID = uID 
  glUniform1f ( unUniformLocation dID ) drawID

  let vc = vertCount ( geometry anEntity ) 
  glDrawElements GL_TRIANGLES ( vc ) GL_UNSIGNED_INT nullPtr
