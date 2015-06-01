module Intersect where

import Data.Vec (Vec3)
import Data.Word (Word8)

import Ray

-- | RGBA
data Color = Color
    {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    deriving (Show, Eq)

data Rayint =
    Miss
  | Hit {
    riDepth :: {-# UNPACK #-} !Float,
    riPos   :: {-# UNPACK #-} !(Vec3 Float),
    riNorm  :: {-# UNPACK #-} !(Vec3 Float),
    riColor :: {-# UNPACK #-} !Color
    }
  | Change {
    riNewRay :: {-# UNPACK #-} !Ray
    } deriving (Show, Eq)

nearest :: Rayint -> Rayint -> Rayint
nearest Miss b = b
nearest a Miss = a
nearest (Change{}) b = b
nearest a (Change{}) = a
nearest a@(Hit da _ _ _) b@(Hit db _ _ _) =
    if da < db
        then a
    else b

isHit :: Rayint -> Bool
isHit Miss = False
isHit _    = True
