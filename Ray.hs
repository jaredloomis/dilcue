{-# LANGUAGE ScopedTypeVariables #-}
module Ray where

import Data.Vec as V
import Control.DeepSeq

data Ray s = Ray !(Vec3 s) !(Vec3 s) deriving (Show, Eq)

instance NFData a => NFData (Ray a) where
    rnf (Ray l r) = l `seq` r `seq` ()

segmentRay :: Floating a => Vec3 a -> Vec3 a -> Ray a
segmentRay from to = Ray from (normalize $ to - from)

rotateRay :: forall s. Floating s => Vec3 s -> Ray s -> Ray s
rotateRay rot (Ray origin (dx:.dy:.dz:.())) =
    let rotMat = rotationEuler rot
        dir4   = rotMat `multmv` (dx:.dy:.dz:.0:.()) :: Vec4 s
        dir3 = V.take n3 dir4
    in Ray origin (normalize dir3)

{-
normalizeRay :: Floating a => Ray a -> Ray a
normalizeRay (Ray origin dir) = Ray origin (normalize dir)
-}
