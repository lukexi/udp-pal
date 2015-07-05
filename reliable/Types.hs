{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE TemplateHaskell           #-}
module Types where
import GHC.Generics (Generic)
import           Graphics.GL
import           Linear
import Data.Binary
import Control.Lens

data Pose = Pose
  { _posPosition    :: V3 GLfloat
  , _posOrientation :: Quaternion GLfloat 
  } deriving (Generic, Binary, Show)
makeLenses ''Pose

type ObjectID = Int

data ObjectOp
  = CreateObject ObjectID Pose
  | NameObject ObjectID String
  deriving (Show, Generic)
instance Binary ObjectOp
data ObjectPose
  = ObjectPose ObjectID Pose
  deriving (Show, Generic)
instance Binary ObjectPose