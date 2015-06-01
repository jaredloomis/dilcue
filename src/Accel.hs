{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnboxedTuples #-}
module Accel where

import Control.Parallel
import qualified Data.DList as D
import Data.List (partition)
import Control.Monad.Par
import Control.Monad.ST
import Data.STRef

import Data.Vec ((:.)(..), Vec3)
import qualified Data.Vec as Vec

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import AABB
import Intersect
import RayTrace
import Ray
import Coherence

----------------------------------------------
-- BIH (Faster than both BVH and LinearBVH) --
----------------------------------------------

data BIH a = BIH (BIHNode a) {-# UNPACK #-} !AABB
  deriving (Show, Eq)

data BIHNode a =
    BIHLeaf [a]
  | BIHNode                !Axis
            {-# UNPACK #-} !Float
            {-# UNPACK #-} !Float
            (BIHNode a) (BIHNode a)
  deriving (Show, Eq)

mkBIH :: HasAABB a => [a] -> BIH a
mkBIH xs = BIH (mkBIH' 0 xs) (createBoundingAll xs)

mkBIH' :: HasAABB a => Int -> [a] -> BIHNode a
mkBIH' depth xs
    | length xs <= 3 = BIHLeaf xs
    |otherwise =
    let (# (# lx,rx #), bb@(AABB bblow bbhigh) #) = splitMidpoint xs X
        (# (# ly,ry #), _ #) = splitMidpoint xs Y
        (# (# lz,rz #), _ #) = splitMidpoint xs Z

        lxmax = maxComp X lx
        rxmin = minComp X rx
        lymax = maxComp Y ly
        rymin = minComp Y ry
        lzmax = maxComp Z lz
        rzmin = minComp Z rz

        objcount = fromIntegral $ length xs
        lxcount  = fromIntegral $ length lx
        rxcount  = fromIntegral $ length rx
        lycount  = fromIntegral $ length ly
        rycount  = fromIntegral $ length ry
        lzcount  = fromIntegral $ length lz
        rzcount  = fromIntegral $ length rz

        mkBBL n val = mkBBL' n val bblow bbhigh
        mkBBR n val = mkBBR' n val bblow bbhigh

        lxbb = mkBBL Vec.n0 lxmax
        rxbb = mkBBR Vec.n0 rxmin
        lybb = mkBBL Vec.n1 lymax
        rybb = mkBBR Vec.n1 rymin
        lzbb = mkBBL Vec.n2 lzmax
        rzbb = mkBBR Vec.n2 rzmin

        costx = (aabbSurfaceArea lxbb * lxcount) +
                (aabbSurfaceArea rxbb * rxcount)
        costy = (aabbSurfaceArea lybb * lycount) +
                (aabbSurfaceArea rybb * rycount)
        costz = (aabbSurfaceArea lzbb * lzcount) +
                (aabbSurfaceArea rzbb * rzcount)
        costorig = aabbSurfaceArea bb * objcount

    in if isMin costorig [costx,costy,costz]
            then BIHLeaf xs
        else if isMin costx [costy,costz]
            then buildBranch lxmax rxmin X lx rx
        else if costy < costz
            then buildBranch lymax rymin Y ly ry
            else buildBranch lzmax rzmin Z lz rz
  where
    buildBranch loff roff axis lobjs robjs =
        if depth > 5
            then BIHNode axis loff roff
                    (mkBIH' (depth+1) lobjs)
                    (mkBIH' (depth+1) robjs)
        else runPar $ do
            leftJ  <- spawn_ (return (mkBIH' (depth+1) lobjs))
            rightJ <- spawn_ (return (mkBIH' (depth+1) robjs))
            lj <- get leftJ
            rj <- get rightJ
            return $ BIHNode axis loff roff lj rj
    mkBBL' n val l h = AABB l (Vec.set n val h)
    mkBBR' n val l = AABB (Vec.set n val l)

    maxComp comp =
        let f x cur = max cur $ component comp (aabbHigh $ boundingBox x)
        in foldr f (-(1/0))
    minComp comp =
        let f x cur = min cur $ component comp (aabbLow $ boundingBox x)
        in foldr f (1/0)

    isMin x (o:os) = x < o && isMin x os
    isMin _ [] = True

rayTraceBIH :: RayTrace a => Ray -> BIH a -> Rayint
rayTraceBIH ray@(Ray (ox:.oy:.oz:.()) dir)
                (BIH root aabb) =
    let (# near, far #) = aabbClip ray aabb
    in traverse near far root
  where
    traverse _ _ (BIHLeaf xs) =
        let rayInts = map (rayTrace ray) xs
        in if null rayInts
            then Miss
            else foldr1 nearest rayInts
    traverse near far (BIHNode axis lsplit rsplit l r) =
        let (# dirr, o #) =
                case axis of
                    X -> (# dxrcp, ox #)
                    Y -> (# dyrcp, oy #)
                    Z -> (# dzrcp, oz #)
            dl = (lsplit - o) * dirr
            dr = (rsplit - o) * dirr
        in if near > far
                then Miss
            else if dirr > 0
                then
                let lInt = if near < dl
                        then traverse near (min dl far) l
                        else Miss
                    rInt = if dr < far
                        then traverse (max dr near) far r
                        else Miss
                in nearest lInt rInt
            else
                let lInt = if near < dr
                        then traverse near (min dr far) r
                        else Miss
                    rInt = if dl < far
                        then traverse (max dl near) far l
                        else Miss
                in nearest lInt rInt

    (dxrcp:.dyrcp:.dzrcp:.()) = Vec.map (1/) dir

rayTracePacketBIH :: RayTrace a => RayPacket -> BIH a -> PacketResult
rayTracePacketBIH
    (RayPacket ray1@(Ray (ox:.oy:.oz:.()) d1)
               ray2 ray3 ray4) (BIH root aabb) =
    let (# near, far #) = aabbClip ray1 aabb
    in case D.toList $ getNearby near far root of
        [] -> PacketResult Miss Miss Miss Miss
        xs  ->
            let i1 = rtAll ray1 xs
                i2 = rtAll ray2 xs
                i3 = rtAll ray3 xs
                i4 = rtAll ray4 xs
            in PacketResult i1 i2 i3 i4
  where
    rtAll r (x:xs) = case rayTrace r x of
        Miss -> rtAll r xs
        intersect -> intersect
    rtAll _ [] = Miss

    getNearby _ _ (BIHLeaf xs) = D.fromList xs
    getNearby near far (BIHNode axis lsplit rsplit l r) =
        let (# dirr, o #) =
                case axis of
                    X -> (# dxrcp, ox #)
                    Y -> (# dyrcp, oy #)
                    Z -> (# dzrcp, oz #)
            dl = (lsplit - o) * dirr
            dr = (rsplit - o) * dirr
        in if near > far
                then D.empty
            else if dirr > 0
                then
                let lInt = if near < dl
                        then getNearby near (min dl far) l
                        else D.empty
                    rInt = if dr < far
                        then getNearby (max dr near) far r
                        else D.empty
                in lInt `D.append` rInt
            else
                let lInt = if near < dr
                        then getNearby near (min dr far) r
                        else D.empty
                    rInt = if dl < far
                        then getNearby (max dl near) far l
                        else D.empty
                in lInt `D.append` rInt

    (dxrcp:.dyrcp:.dzrcp:.()) = Vec.map (1/) d1

instance RayTrace a => RayTrace (BIH a) where
    rayTrace = rayTraceBIH
    rayTracePacket = rayTracePacketBIH

---------------------------------------------
-- LINEAR BVH (Faster than normal BVH) (?) --
---------------------------------------------

type LinearBVH a = V.Vector (LinearBVHNode a)
type MLinearBVH st a = MV.STVector st (LinearBVHNode a)

data LinearBVHNode a =
    LinearBVHNode                !Axis
                  {-# UNPACK #-} !AABB
                  {-# UNPACK #-} !Int
                  {-# UNPACK #-} !Int
  | LinearBVHLeaf !Axis a
  deriving (Show, Eq, Functor)

rayTraceLBVH :: RayTrace a => Ray -> LinearBVH a -> Rayint
rayTraceLBVH ray@(Ray _ direction) vec = rayTrace' (vec `V.unsafeIndex` 0)
  where
    rayTrace' (LinearBVHLeaf _ x) = rayTrace ray x
    rayTrace' (LinearBVHNode axis aabb left right) =
        if isHit $ rayTrace ray aabb
            then nearest firstInt otherInt
        else Miss
      where
        (# childa, childb #) =
            if component axis direction > 0
                then (# left, right #) else (# right, left #)
        firstInt = rayTrace' (vec `V.unsafeIndex` childa)
        otherInt = rayTrace' (vec `V.unsafeIndex` childb)

rayTracePacketLBVH :: RayTrace a => RayPacket -> LinearBVH a -> PacketResult
rayTracePacketLBVH (RayPacket ray1@(Ray _ d1) ray2 ray3 ray4) vec =
    rayTrace' (vec `V.unsafeIndex` 0)
  where
    getNearby (LinearBVHLeaf _ a) = D.singleton a
    getNearby (LinearBVHNode axis aabb left right) =
        if isHit $ rayTrace ray1 aabb
            then dlist
        else D.empty
      where
        dlist =
            let other = getNearby (vec `V.unsafeIndex` childb)
                first = other `par` getNearby (vec `V.unsafeIndex` childa)
            in first `D.append` other
        (# childa, childb #) =
            if component axis d1 > 0
                then (# left, right #) else (# right, left #)
    rayTrace' bvh =
        case D.toList $ getNearby bvh of
            [] -> PacketResult Miss Miss Miss Miss
            a  ->
                let i1 = rtAll ray1 a
                    i2 = rtAll ray2 a
                    i3 = rtAll ray3 a
                    i4 = rtAll ray4 a
                in PacketResult i1 i2 i3 i4

    rtAll r = foldr (nearest . rayTrace r) Miss

instance RayTrace a => RayTrace (LinearBVH a) where
    rayTrace = rayTraceLBVH
    rayTracePacket = rayTracePacketLBVH

flatten :: BVH a -> LinearBVH a
flatten bvh = runST $ do
    iref <- newSTRef 0
    vec <- MV.new (bvhLinearLen bvh)
    vec' <- flattenST iref vec bvh
    V.unsafeFreeze vec'

flattenST :: STRef st Int -> MLinearBVH st a -> BVH a ->
             ST st (MLinearBVH st a)
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

bvhLinearLen :: BVH a -> Int
bvhLinearLen (Node _ _ l r) =
    1 + bvhLinearLen l + bvhLinearLen r
bvhLinearLen _ = 1

---------------------------
-- Normal Tree-style BVH --
---------------------------

data BVH a =
    Node !Axis {-# UNPACK #-} !AABB
         (BVH a) (BVH a)
  | Leaf !Axis {-# UNPACK #-} !AABB
         {-# UNPACK #-} !Int [a]
  deriving (Show, Eq, Functor)

rayTraceBVH :: RayTrace a => Ray -> BVH a -> Rayint
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

rayTracePacketBVH :: RayTrace a => RayPacket -> BVH a -> PacketResult
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

instance RayTrace a => RayTrace (BVH a) where
    rayTrace = rayTraceBVH
    rayTracePacket = rayTracePacketBVH

bvhToList :: BVH a -> [a]
bvhToList = D.toList . bvhToListD
  where
    bvhToListD :: BVH a -> D.DList a
    bvhToListD (Node _ _ l r) =
        bvhToListD l `D.append` bvhToListD r
    bvhToListD (Leaf _ _ _ xs) = D.fromList xs

mkBVH :: HasAABB a => [a] -> BVH a
mkBVH [x] = Leaf X (boundingBox x) 1 [x]
mkBVH []  = Leaf X emptyAABB 0 []
mkBVH xs  =
    Node dim allBounds (mkBVH left) (mkBVH right)
  where
    (# (# left, right #), allBounds #) = splitMidpoint xs dim
    dim = maximumExtent . centroidBounds $ xs

intersectBVH :: RayTrace a => Ray -> BVH a -> Bool
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

nearby :: Ray -> BVH a -> [a]
nearby ray = D.toList . nearbyD ray

nearbyD :: Ray -> BVH a -> D.DList a
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

centroidBounds :: HasAABB a => [a] -> AABB
centroidBounds = foldr (expandAABBToFit . boundingBox) emptyAABB

splitMidpoint :: HasAABB a => [a] -> Axis -> (# (# [a],[a] #), AABB #)
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
                              --volumeCentroid allbounds
    allBounds = foldr expandAABBToFit emptyAABB xs

volumeCentroid :: HasAABB a => [a] -> Vec3 Float
volumeCentroid = foldr choose 0
  where
    choose this cur =
        avgVec cur $ aabbCentroid (boundingBox this)

    avgVec (x:.y:.z:.()) (a:.b:.c:.()) =
        avg x a:.avg y b:. avg z c:.()
    avg x y = (x+y) / 2
