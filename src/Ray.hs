module Ray where

import Data.Vec as V

data Ray = Ray {-# UNPACK #-} !(Vec3 Float)
               {-# UNPACK #-} !(Vec3 Float)
  deriving (Show, Eq)

segmentRay :: Vec3 Float -> Vec3 Float -> Ray
segmentRay from to = Ray from (normalize $ to - from)

rotateRay :: Vec3 Float -> Ray -> Ray
rotateRay rot (Ray origin (dx:.dy:.dz:.())) =
    let rotMat = rotationEuler rot
        dir4   = rotMat `multmv` (dx:.dy:.dz:.0:.())
        dir3 = V.take n3 dir4
    in Ray origin (normalize dir3)
