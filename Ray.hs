module Ray where

import Data.Vec
import Control.DeepSeq

data Ray a = Ray !(Vec3 a) !(Vec3 a) deriving (Show, Eq)

instance NFData a => NFData (Ray a) where
    rnf (Ray l r) = l `seq` r `seq` ()

segmentRay :: Floating a => Vec3 a -> Vec3 a -> Ray a
segmentRay from to = Ray from (normalize $ to - from)

{-
normalizeRay :: Floating a => Ray a -> Ray a
normalizeRay (Ray origin dir) = Ray origin (normalize dir)
-}
