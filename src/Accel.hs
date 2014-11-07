{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnboxedTuples #-}
module Accel where

import qualified Data.DList as D
import Data.List (partition)
import Control.Monad.ST
import Data.STRef

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import AABB
import Intersect
import RayTrace
import Ray
import Coherence

-----------------------------------------
-- LINEAR BVH (Faster than normal BVH) --
-----------------------------------------

type LinearBVH s a = V.Vector (LinearBVHNode s a)
type MLinearBVH st s a = MV.STVector st (LinearBVHNode s a)

data LinearBVHNode s a =
    LinearBVHNode !Axis !(AABB s) {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | LinearBVHLeaf !Axis a
  deriving (Show, Eq, Functor)

rayTraceLBVH :: (RayTrace s a, Ord s, Fractional s) =>
    Ray s -> LinearBVH s a -> Rayint s
rayTraceLBVH ray@(Ray _ direction) vec = rayTrace' (vec `V.unsafeIndex` 0)
  where
    rayTrace' (LinearBVHLeaf _ x) = rayTrace ray x
    rayTrace' (LinearBVHNode axis aabb left right) =
        if isHit $ rayTrace ray aabb
            then if isHit firstInt
                then firstInt
            else otherInt
        else Miss
      where
        (# childa, childb #) =
            if component axis direction > 0
                then (# left, right #) else (# right, left #)
        firstInt = rayTrace' (vec `V.unsafeIndex` childa)
        otherInt = rayTrace' (vec `V.unsafeIndex` childb)
{-# SPECIALIZE
 rayTraceLBVH :: RayTrace Float a =>
                 Ray Float -> LinearBVH Float a -> Rayint Float
 #-}

rayTracePacketLBVH :: (RayTrace s a, Fractional s, Ord s) =>
    RayPacket s -> LinearBVH s a -> PacketResult s
rayTracePacketLBVH (RayPacket ray1@(Ray _ d1) ray2 ray3 ray4) vec =
    rayTrace' (vec `V.unsafeIndex` 0)
  where
    getNearby (LinearBVHLeaf _ a) = D.singleton a
    getNearby (LinearBVHNode axis aabb left right) =
        if isHit $ rayTrace ray1 aabb
            then firstInt `D.append` otherInt
        else D.empty
      where
        (# childa, childb #) =
            if component axis d1 > 0
                then (# left, right #) else (# right, left #)
        firstInt = getNearby (vec `V.unsafeIndex` childa)
        otherInt = getNearby (vec `V.unsafeIndex` childb)

    rayTrace' bvh =
        case D.toList $ getNearby bvh of
            [] -> PacketResult Miss Miss Miss Miss
            a  ->
                let i1 = rtAll ray1 a
                    i2 = rtAll ray2 a
                    i3 = rtAll ray3 a
                    i4 = rtAll ray4 a
                in PacketResult i1 i2 i3 i4

    rtAll r (x:xs) = case rayTrace r x of
        Miss -> rtAll r xs
        intersect -> intersect
    rtAll _ [] = Miss
{-# SPECIALIZE
 rayTracePacketLBVH :: RayTrace Float a =>
    RayPacket Float -> LinearBVH Float a -> PacketResult Float
 #-}

instance (RayTrace s a, Ord s, Fractional s) =>
          RayTrace s (LinearBVH s a) where
    rayTrace = rayTraceLBVH
    rayTracePacket = rayTracePacketLBVH

flatten :: BVH s a -> LinearBVH s a
flatten bvh = runST $ do
    iref <- newSTRef 0
    vec <- MV.new (bvhLinearLen bvh)
    vec' <- flattenST iref vec bvh
    V.unsafeFreeze vec'

flattenST :: STRef st Int -> MLinearBVH st s a -> BVH s a ->
             ST st (MLinearBVH st s a)
flattenST iref vec (Leaf axis _ _ xs) =
    let linear = LinearBVHLeaf axis (head xs)
    in do
        i <- readSTRef iref
        MV.unsafeWrite vec i linear
        writeSTRef iref (i+1)
        return vec
flattenST iref vec (Node axis aabb left right) = do
    thisI <- readSTRef iref
    writeSTRef iref (thisI + 1)

    vec' <- flattenST iref vec left
    leftI <- readSTRef iref

    vec'' <- flattenST iref vec' right
    rightI <- (\x->x-1) `fmap` readSTRef iref

    let thisNode = LinearBVHNode axis aabb (thisI+1) leftI
    MV.unsafeWrite vec'' thisI thisNode
    writeSTRef iref (rightI+1)
    return vec''

bvhLinearLen :: BVH s a -> Int
bvhLinearLen (Node _ _ l r) =
    1 + bvhLinearLen l + bvhLinearLen r
bvhLinearLen _ = 1

---------------------------
-- Normal Tree-style BVH --
---------------------------

data BVH s a =
    Node !Axis !(AABB s) (BVH s a) (BVH s a)
  | Leaf !Axis !(AABB s) {-# UNPACK #-} !Int [a]
  deriving (Show, Eq, Functor)

rayTraceBVH :: (RayTrace s a, Ord s, Fractional s) =>
    Ray s -> BVH s a -> Rayint s
rayTraceBVH _ (Leaf _ _ 0 _) = Miss
rayTraceBVH ray (Leaf _ aabb _ xs) =
        if isHit $ rayTrace ray aabb
            then
                let casts = map (rayTrace ray) xs
                    hits = filter isHit casts
                in if null hits
                    then Miss
                    else head hits -- TODO: Choose the correct hit
                                   -- (The one closest to ray origin.)
        else Miss
rayTraceBVH ray@(Ray _ direction) (Node axis aabb left right) =
        if isHit $ rayTrace ray aabb
            -- TODO: Shouldn't stop at first hit, should
            -- collect all hits, then find one closest to
            -- the eye/origin.
            then if isHit firstInt
                    then firstInt
                else otherInt
        else Miss
      where
        (childa, childb) =
            if component axis direction > 0
                then (left, right) else (right, left)
        firstInt = rayTrace ray childa
        otherInt = rayTrace ray childb
{-# SPECIALIZE
 rayTraceBVH :: RayTrace Float a =>
                Ray Float -> BVH Float a -> Rayint Float
 #-}

rayTracePacketBVH :: (RayTrace s a, Fractional s, Ord s) =>
    RayPacket s -> BVH s a -> PacketResult s
rayTracePacketBVH (RayPacket ray1@(Ray _ d1) ray2 ray3 ray4) bvh =
    case D.toList $ getNearby bvh of
        [] -> PacketResult Miss Miss Miss Miss
        a  ->
            let i1 = rtAll ray1 a
                i2 = rtAll ray2 a
                i3 = rtAll ray3 a
                i4 = rtAll ray4 a
            in PacketResult i1 i2 i3 i4
  where
    getNearby (Leaf _ _ _ xs) = D.fromList xs
    getNearby (Node axis aabb left right) =
        if isHit $ rayTrace ray1 aabb
            then firstInt `D.append` otherInt
        else D.empty
      where
        (# childa, childb #) =
            if component axis d1 > 0
                then (# left, right #) else (# right, left #)
        firstInt = getNearby childa
        otherInt = getNearby childb

    rtAll r (x:xs) = case rayTrace r x of
        Miss -> rtAll r xs
        intersect -> intersect
    rtAll _ [] = Miss
{-# SPECIALIZE
 rayTracePacketBVH :: RayTrace Float a =>
    RayPacket Float -> BVH Float a -> PacketResult Float
 #-}

instance (RayTrace s a, Ord s, Fractional s) => RayTrace s (BVH s a) where
    rayTrace = rayTraceBVH
    rayTracePacket = rayTracePacketBVH

bvhToList :: BVH s a -> [a]
bvhToList = D.toList . bvhToListD
  where
    bvhToListD :: BVH s a -> D.DList a
    bvhToListD (Node _ _ l r) =
        bvhToListD l `D.append` bvhToListD r
    bvhToListD (Leaf _ _ _ xs) = D.fromList xs

mkBVH :: (HasAABB s a, Ord s, Fractional s) => [a] -> BVH s a
mkBVH [x] = Leaf X (boundingBox x) 1 [x]
mkBVH []  = Leaf X emptyAABB 0 []
mkBVH xs  =
    Node dim allBounds (mkBVH left) (mkBVH right)
  where
    (# (# left, right #), allBounds #) = splitMidpoint xs dim
    dim = maximumExtent . centroidBounds $ xs
{-# SPECIALIZE mkBVH :: HasAABB Float a => [a] -> BVH Float a #-}

intersectBVH :: (RayTrace s a, Ord s, Fractional s) =>
                 Ray s -> BVH s a -> Bool
intersectBVH _ (Leaf _ _ 0 _) = False
intersectBVH ray (Leaf _ aabb _ xs) =
    isHit (rayTrace ray aabb) &&
        any (isHit . rayTrace ray) xs
intersectBVH ray@(Ray _ direction) (Node axis aabb left right) =
    isHit (rayTrace ray aabb) &&
        (firstInt || otherInt)
  where
    (childa, childb) =
        if component axis direction > 0
            then (left,right) else (right,left)
    firstInt = intersectBVH ray childa
    otherInt = intersectBVH ray childb
{-# SPECIALIZE
 intersectBVH :: RayTrace Float a => Ray Float -> BVH Float a -> Bool
 #-}

nearby :: (Ord s, Fractional s) => Ray s -> BVH s a -> [a]
nearby ray = D.toList . nearbyD ray
{-# SPECIALIZE nearby :: Ray Float -> BVH Float a -> [a] #-}

nearbyD :: (Ord s, Fractional s) => Ray s -> BVH s a -> D.DList a
nearbyD _ (Leaf _ _ 0 _) = D.empty
nearbyD x (Leaf _ aabb _ xs) =
    if isHit $ rayTrace x aabb then D.fromList xs else D.empty
nearbyD x@(Ray _ direction) (Node axis aabb left right) =
    if not . isHit $ rayTrace x aabb
        then D.empty
    else firstInt `D.append` otherInt
  where
    (childa, childb) =
        if component axis direction > 0
            then (left,right) else (right,left)
    firstInt = nearbyD x childa
    otherInt = nearbyD x childb
{-# SPECIALIZE nearbyD :: Ray Float -> BVH Float a -> D.DList a #-}

centroidBounds :: (HasAABB s a, Fractional s, Ord s) => [a] -> AABB s
centroidBounds = foldr (expandAABBToFit . boundingBox) emptyAABB
{-# SPECIALIZE centroidBounds :: HasAABB Float a => [a] -> AABB Float #-}

splitMidpoint :: (HasAABB s a, Fractional s, Ord s) =>
    [a] -> Axis -> (# (# [a],[a] #), AABB s #)
splitMidpoint xs axis =
    let (ls, rs) = partition isLeft xs
    in if null ls && not (null rs)
            then (# splitMiddle rs, allBounds #)
        else (# (# ls, rs #), allBounds #)
  where
    splitMiddle ys =
        let (l, r) = splitAt (length ys `div` 2) ys
        in (# l, r #)
    isLeft x = component axis (aabbCentroid $ boundingBox x) < midpoint
    midpoint = component axis $ aabbCentroid allBounds
    allBounds = foldr expandAABBToFit emptyAABB xs
{-# SPECIALIZE
 splitMidpoint :: HasAABB Float a =>
         [a] -> Axis -> (# (# [a],[a] #), AABB Float #)
 #-}
