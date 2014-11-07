module Intersect where

import Data.Vec (Vec3)
import Data.Word (Word8)

import Ray

-- | RGBA
data Color = Color
    {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    deriving (Show, Eq)

data Rayint s =
    Miss
  | Hit {
    riDepth :: s,
    riPos   :: Vec3 s,
    riNorm  :: Vec3 s,
    riColor :: !Color
    }
  | Change {
    riNewRay :: !(Ray s)
    } deriving (Show, Eq)

isHit :: Rayint s -> Bool
isHit Miss = False
isHit _    = True
