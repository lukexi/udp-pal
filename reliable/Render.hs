{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE DeriveDataTypeable        #-}
module Render where
import           Control.Monad.State


import           Control.Concurrent.STM
import qualified Data.Map               as Map
--import           Data.Map               (Map)
import           Control.Lens hiding (view)
import           Graphics.UI.GLFW.Pal
import           Graphics.GL.Pal2
import           Graphics.GL
import           Linear

import Data.Maybe
import Data.Data

import Types

data Uniforms = Uniforms
  { uViewProjection :: UniformLocation (M44 GLfloat)
  , uInverseModel   :: UniformLocation (M44 GLfloat)
  , uModel          :: UniformLocation (M44 GLfloat)
  , uCamLocation    :: UniformLocation (V3 GLfloat)
  , uDiffuse        :: UniformLocation (V4 GLfloat)
  } deriving (Data, Typeable)

resX, resY :: Int
resX=1024; resY=768

initRenderer :: IO (Window, Events, Entity Uniforms)
initRenderer = do
  (window, events) <- createWindow "ReliableCubes" resX resY

  cubeProg   <- createShaderProgram "reliable/poly.vert" "reliable/poly.frag"
  cubeGeo    <- cubeGeometry ( 0.5 :: V3 GLfloat ) ( V3 1 1 1 )
  cube       <- entity cubeGeo cubeProg 

  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  glEnable GL_CULL_FACE
  glCullFace GL_BACK

  useProgram (program cube)

  return (window, events, cube)

renderFrame :: (MonadIO m, MonadState AppState m) 
            => Window -> Entity Uniforms -> m ()
renderFrame window cube = do
  let Uniforms{..} = uniforms cube
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

  projection  <- makeProjection window
  let playerPos      = V3 0 0 5
      playerOrient   = axisAngle (V3 0 1 0) 0
      view           = viewMatrix playerPos playerOrient
      projectionView = projection !*! view
  uniformV3 uCamLocation playerPos

  newCubes   <- use cubePoses
  newPlayers <- use playerPoses
  withVAO ( vAO cube ) $ do
    forM_ ( Map.toList newCubes ) $ \( objID , pose ) -> do

      let model = mkTransformation (pose ^. posOrientation) (pose ^. posPosition)
      color <- fromMaybe (V4 0 1 0 1) <$> use (cubeColors . at objID)

      drawEntity cube model projectionView color
    forM_ ( Map.toList newPlayers ) $ \( playerID , pose ) -> do

      let model = mkTransformation (pose ^. posOrientation) (pose ^. posPosition)
      color <- fromMaybe (V4 0 1 0 1) <$> use (playerColors . at playerID)

      drawEntity cube model projectionView color

  swapBuffers window

drawEntity :: MonadIO m => Entity Uniforms -> M44 GLfloat -> M44 GLfloat -> Color -> m ()
drawEntity anEntity model projectionView color = do 

  let Uniforms{..} = uniforms anEntity

  uniformM44 uViewProjection projectionView
  uniformM44 uInverseModel (fromMaybe model (inv44 model))
  uniformM44 uModel model
  uniformV4  uDiffuse color

  let vc = vertCount ( geometry anEntity ) 
  glDrawElements GL_TRIANGLES ( vc ) GL_UNSIGNED_INT nullPtr

-- | Get a view matrix for a camera at a given position and orientation
viewMatrix :: (RealFloat a, Conjugate a) => V3 a -> Quaternion a -> M44 a
viewMatrix position orientation = mkTransformation q (rotate q . negate $ position)
    where q = conjugate orientation

-- | Use the aspect ratio from the window to get a proper projection
makeProjection :: (Floating a, MonadIO m) => Window -> m (M44 a)
makeProjection win = do
    (w,h) <- getWindowSize win
    return $ perspective 45 (fromIntegral w / fromIntegral h) 0.01 100


