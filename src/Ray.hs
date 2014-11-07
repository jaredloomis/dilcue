module Ray where

import Data.Vec as V

data Ray s = Ray !(Vec3 s) !(Vec3 s) deriving (Show, Eq)

segmentRay :: Floating a => Vec3 a -> Vec3 a -> Ray a
segmentRay from to = Ray from (normalize $ to - from)

rotateRay :: Floating s => Vec3 s -> Ray s -> Ray s
rotateRay rot (Ray origin (dx:.dy:.dz:.())) =
    let rotMat = rotationEuler rot
        dir4   = rotMat `multmv` (dx:.dy:.dz:.0:.())
        dir3 = V.take n3 dir4
    in Ray origin (normalize dir3)
