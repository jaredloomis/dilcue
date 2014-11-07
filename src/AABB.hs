{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module AABB where

import Data.Vec

data AABB a = AABB {
    aabbLow :: Vec3 a,
    aabbHigh :: Vec3 a
    } deriving (Eq, Ord)

emptyAABB :: (Fractional a, Ord a) => AABB a
emptyAABB = AABB infinity (-infinity)
  where
    infinity :: Fractional a => a
    infinity = 1 / 0

instance Show a => Show (AABB a) where
    show (AABB l h) =
        "AABB " ++ show l ++ " " ++ show h

data Axis = X | Y | Z deriving (Show, Eq, Ord)

component :: Axis -> Vec3 a -> a
component X (x:._) = x
component Y (_:.y:._) = y
component Z (_:._:.z:._) = z

-- | This only allows one instance per type,
--   so I may want to change this. Also, Should I
--   use TypeFamilies instead? (They are faster?)
class HasAABB s a | a -> s where
    boundingBox :: a -> AABB s

instance HasAABB s (AABB s) where
    boundingBox = id

expandAABBToFit :: (HasAABB s a, Ord s) => a -> AABB s -> AABB s
expandAABBToFit x (AABB (clx:.cly:.clz:.()) (chx:.chy:.chz:.())) =
    let AABB (xlx:.xly:.xlz:.()) (xhx:.xhy:.xhz:.()) = boundingBox x

        nlx = min xlx clx
        nly = min xly cly
        nlz = min xlz clz

        nhx = max xhx chx
        nhy = max xhy chy
        nhz = max xhz chz
    in AABB (nlx:.nly:.nlz:.()) (nhx:.nhy:.nhz:.())

aabbIntersect :: (HasAABB s a, HasAABB s b, Ord s) => a -> b -> Bool
aabbIntersect l r =
    let lbox = boundingBox l
        rbox = boundingBox r
    in aabbIntersect' lbox rbox

aabbIntersect' :: Ord s => AABB s -> AABB s -> Bool
aabbIntersect' (AABB (lax:.lay:.laz:.()) (hax:.hay:.haz:.()))
               (AABB (lbx:.lby:.lbz:.()) (hbx:.hby:.hbz:.())) =
    hax >= lbx &&
    lax <= hbx &&
    hay >= lby &&
    lay <= hby &&
    haz >= lbz &&
    laz <= hbz

moveAABB :: Num s => AABB s -> Vec3 s -> AABB s
moveAABB (AABB l h) d = AABB (l+d) (h+d)

aabbCentroid :: (Fractional s, Ord s) => AABB s -> Vec3 s
aabbCentroid (AABB l h) = (h+l) * 0.5

maximumExtent :: (Ord s, Num s) => AABB s -> Axis
maximumExtent (AABB l h) = dominant $ h - l
  where
    dominant (x:.y:.z:.())
        | (abs x > abs y) && (abs x > abs z) = X
        | abs y > abs z = Y
        | otherwise = Z

pointInAABB :: (Ord s) => Vec3 s -> AABB s -> Bool
pointInAABB (px:.py:.pz:.())
            (AABB (lx:.ly:.lz:.()) (hx:.hy:.hz:.())) =
    px >= lx && px <= hx &&
    py >= ly && py <= hy &&
    pz >= lz && pz <= hz
