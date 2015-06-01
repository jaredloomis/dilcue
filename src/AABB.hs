{-# LANGUAGE UnboxedTuples #-}
module AABB where

import Data.Vec as V

import Ray

data AABB = AABB {
    aabbLow  :: {-# UNPACK #-} !(Vec3 Float),
    aabbHigh :: {-# UNPACK #-} !(Vec3 Float)
    } deriving (Eq, Ord)

emptyAABB :: AABB
emptyAABB = AABB infinity (-infinity)
  where
    infinity :: Vec3 Float
    infinity = 1 / 0

instance Show AABB where
    show (AABB l h) =
        "AABB " ++ show l ++ " " ++ show h

data Axis = X | Y | Z deriving (Show, Eq, Ord)

component :: Axis -> Vec3 a -> a
component X (x:._) = x
component Y (_:.y:._) = y
component Z (_:._:.z:._) = z

class HasAABB a where
    boundingBox :: a -> AABB

instance HasAABB AABB where
    boundingBox = id

expandAABBToFit :: HasAABB a => a -> AABB -> AABB
expandAABBToFit x (AABB (clx:.cly:.clz:.()) (chx:.chy:.chz:.())) =
    let AABB (xlx:.xly:.xlz:.()) (xhx:.xhy:.xhz:.()) = boundingBox x

        nlx = min xlx clx
        nly = min xly cly
        nlz = min xlz clz

        nhx = max xhx chx
        nhy = max xhy chy
        nhz = max xhz chz
    in AABB (nlx:.nly:.nlz:.()) (nhx:.nhy:.nhz:.())

createBoundingAll :: HasAABB a => [a] -> AABB
createBoundingAll =
    Prelude.foldr expandAABBToFit emptyAABB

aabbSurfaceArea :: AABB -> Float
aabbSurfaceArea (AABB l h) =
    let (dx:.dy:.dz:.()) = h - l
    in max 0 $ 2*(dx*dy + dx*dz + dy*dz)

aabbIntersect :: (HasAABB a, HasAABB b) => a -> b -> Bool
aabbIntersect l r =
    let lbox = boundingBox l
        rbox = boundingBox r
    in aabbIntersect' lbox rbox

aabbIntersect' :: AABB -> AABB -> Bool
aabbIntersect' (AABB (lax:.lay:.laz:.()) (hax:.hay:.haz:.()))
               (AABB (lbx:.lby:.lbz:.()) (hbx:.hby:.hbz:.())) =
    hax >= lbx &&
    lax <= hbx &&
    hay >= lby &&
    lay <= hby &&
    haz >= lbz &&
    laz <= hbz

moveAABB :: AABB -> Vec3 Float -> AABB
moveAABB (AABB l h) d = AABB (l+d) (h+d)

aabbCentroid :: AABB -> Vec3 Float
aabbCentroid (AABB l h) = (h+l) * 0.5

maximumExtent :: AABB -> Axis
maximumExtent (AABB l h) = dominant $ h - l
  where
    dominant (x:.y:.z:.())
        | (abs x > abs y) && (abs x > abs z) = X
        | abs y > abs z = Y
        | otherwise = Z

pointInAABB :: Vec3 Float -> AABB -> Bool
pointInAABB (px:.py:.pz:.())
            (AABB (lx:.ly:.lz:.()) (hx:.hy:.hz:.())) =
    px >= lx && px <= hx &&
    py >= ly && py <= hy &&
    pz >= lz && pz <= hz

aabbClip :: Ray -> AABB -> (# Float, Float #)
aabbClip (Ray (ox:.oy:.oz:.()) dir@(dx:.dy:.dz:.()))
       (AABB (lx:.ly:.lz:.()) (hx:.hy:.hz:.())) =
    let (dxrcp:.dyrcp:.dzrcp:.()) = V.map (1/) dir
        (# inx, outx #) =
            if dx > 0
                then (# (lx-ox)*dxrcp, (hx-ox)*dxrcp #)
                else (# (hx-ox)*dxrcp, (lx-ox)*dxrcp #)
        (# iny, outy #) =
            if dy > 0
                then (# (ly-oy)*dyrcp, (hy-oy)*dyrcp #)
                else (# (hy-oy)*dyrcp, (ly-oy)*dyrcp #)
        (# inz, outz #) =
            if dz > 0
                then (# (lz-oz)*dzrcp, (hz-oz)*dzrcp #)
                else (# (hz-oz)*dzrcp, (lz-oz)*dzrcp #)
    in (# max (max inx iny) inz,
          min (min outx outy) outz #)
