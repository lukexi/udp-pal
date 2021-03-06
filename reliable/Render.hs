{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE DeriveDataTypeable        #-}
module Render where
import           Control.Monad.State.Strict


import qualified Data.Map.Strict               as Map
import           Control.Lens hiding (view)
import           Graphics.UI.GLFW.Pal
import           Graphics.GL.Pal

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

initRenderer :: IO (Window, Events, Shape Uniforms)
initRenderer = do
  (window, events) <- createWindow "ReliableCubes" resX resY

  cubeProg   <- createShaderProgram "reliable/poly.vert" "reliable/poly.frag"
  cubeGeo    <- cubeGeometry (0.5 :: V3 GLfloat) (V3 1 1 1)
  cubeShape  <- makeShape cubeGeo cubeProg 

  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  glEnable GL_CULL_FACE
  glCullFace GL_BACK

  useProgram (sProgram cubeShape)

  return (window, events, cubeShape)

renderFrame :: (MonadIO m, MonadState AppState m) 
            => Window -> Shape Uniforms -> m ()
renderFrame window cube = do
  let Uniforms{..} = sUniforms cube
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

  projection  <- getWindowProjection window 45 0.1 100
  let playerPos      = V3 0 0 5
      playerOrient   = axisAngle (V3 0 1 0) 0
      view           = viewMatrix playerPos playerOrient
      projectionView = projection !*! view
  uniformV3 uCamLocation playerPos

  newCubes   <- use cubePoses
  newPlayers <- use playerPoses
  withVAO (sVAO cube) $ do
    forM_ (Map.toList newCubes) $ \(objID , pose) -> do

      let model = mkTransformation (pose ^. posOrientation) (pose ^. posPosition)
      color <- fromMaybe (V4 0 1 0 1) <$> use (cubeColors . at objID)

      drawShape' cube model projectionView color
    forM_ (Map.toList newPlayers) $ \(playerID , pose) -> do

      let model = mkTransformation (pose ^. posOrientation) (pose ^. posPosition)
      color <- fromMaybe (V4 0 1 0 1) <$> use (playerColors . at playerID)

      drawShape' cube model projectionView color

  swapBuffers window

drawShape' :: MonadIO m => Shape Uniforms -> M44 GLfloat -> M44 GLfloat -> Color -> m ()
drawShape' shape model projectionView color = do 

  let Uniforms{..} = sUniforms shape

  uniformM44 uViewProjection projectionView
  uniformM44 uInverseModel (fromMaybe model (inv44 model))
  uniformM44 uModel model
  uniformV4  uDiffuse color

  let vc = geoVertCount (sGeometry shape) 
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr
