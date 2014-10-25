{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
module BVH where

--import Control.Parallel
import qualified Data.DList as D
import Data.List (partition)
import Data.Vec ((:.)(..), Vec3)

import AABB
import Intersect
import Ray

-- OCTREE (SLOWER)

data Octree s a =
    ONode !(AABB s) [Octree s a]
  | OLeaf !(AABB s) [a] {-# UNPACK #-} !Int
  deriving (Show, Eq)

instance (RayTrace s a, Ord s, Fractional s) => RayTrace s (Octree s a) where
    rayTrace _ (OLeaf _ _ 0) = Miss
    rayTrace r (OLeaf aabb xs _)
        | isHit $ rayTrace r aabb =
            let ints = map (rayTrace r) xs
                hits = filter isHit ints
            in if null hits
                then Miss
                else head hits -- TODO: Choose correct hit.
        | otherwise = Miss
    rayTrace r (ONode aabb xs)
        | isHit $ rayTrace r aabb =
            let ints = map (rayTrace r) xs
                hits = filter isHit ints
            in if null hits
                then Miss
                else head hits -- TODO: Choose correct hit.
        | otherwise = Miss

maxCapacity :: Int
maxCapacity = 16
{-# INLINE maxCapacity #-}

mkOctree :: (HasAABB s a, Eq a, Ord s, Fractional s) =>
             AABB s -> [a] -> Octree s a
mkOctree aabb = foldr octInsert (OLeaf aabb [] 0)

octInsert :: (HasAABB s a, Eq a, Ord s, Fractional s) =>
              a -> Octree s a -> Octree s a
octInsert val (ONode aabb children) =
    let (insertIntos, others) = dpartition (checkOctant val) children
    in  -- I would do this, but doing a comparison with
        -- a DList forces it to turn into a list, which
        -- causes this function to be slower than the
        -- list-only version.
        -- > if insertIntos == D.empty
        -- >    then error "Collision.octInsert..."
        -- > else ...
        ONode aabb . D.toList . (`D.append` others) .
             D.map (octInsert val) $ insertIntos
octInsert val leaf@(OLeaf aabb contents size) =
    if size < maxCapacity
        then OLeaf aabb (val : contents) (size+1)
    else octInsert val (subdivide leaf)

findNearby :: (HasAABB s a, Ord s) => a -> Octree s a -> [a]
findNearby val (ONode _ children) =
    let insertIntos = filter (checkOctant val) children
    in if null insertIntos
            then []
        else concatMap (findNearby val) insertIntos
findNearby _ (OLeaf _ contents _) = contents

dpartition :: (a -> Bool) -> [a] -> (D.DList a, D.DList a)
dpartition p = foldr (dselect p) (D.empty, D.empty)
{-# INLINE dpartition #-}

dselect :: (a -> Bool) -> a -> (D.DList a, D.DList a) -> (D.DList a, D.DList a)
dselect p x (ts, fs) | p x = (x `D.cons` ts,fs)
                     | otherwise = (ts, x `D.cons` fs)

subdivide :: (HasAABB s a, Eq a, Ord s, Fractional s) =>
              Octree s a -> Octree s a
subdivide (OLeaf wholeAABB@(AABB minVec maxVec) contents _) =
    let halfVec@(halfX :. halfY :. halfZ :. ()) =
            abs (maxVec - minVec) / 2
        newAABBTemplate = AABB minVec $ minVec + halfVec 

        northWestA = newAABBTemplate
        northWestB = moveAABB northWestA (0 :. 0 :. halfZ :. ())
        northWestALeaf = OLeaf northWestA [] 0
        northWestBLeaf = OLeaf northWestB [] 0
        
        northEastA = moveAABB newAABBTemplate (halfX :. 0 :. 0 :. ())
        northEastB = moveAABB northEastA (0 :. 0 :. halfZ :. ())
        northEastALeaf = OLeaf northEastA [] 0
        northEastBLeaf = OLeaf northEastB [] 0

        southWestA = moveAABB newAABBTemplate (0 :. halfY :. 0 :. ())
        southWestB = moveAABB southWestA (0 :. 0 :. halfZ :. ())
        southWestALeaf = OLeaf southWestA [] 0
        southWestBLeaf = OLeaf southWestB [] 0

        southEastA = moveAABB newAABBTemplate (halfX :. halfY :. 0 :. ())
        southEastB = moveAABB southEastA (0 :. 0 :. halfZ :. ())
        southEastALeaf = OLeaf southEastA [] 0
        southEastBLeaf = OLeaf southEastB [] 0
 
        newNode = ONode wholeAABB [northWestALeaf, northWestBLeaf,
                                   northEastALeaf, northEastBLeaf,
                                   southWestALeaf, southWestBLeaf,
                                   southEastALeaf, southEastBLeaf]
    in foldr octInsert newNode contents
subdivide _ = error "Collision.subdivide: cannot subdivide a ONode."

checkOctant :: (HasAABB s a, Ord s) => a -> Octree s a -> Bool
checkOctant val (ONode aabb _) =
    aabbIntersect val aabb
checkOctant val (OLeaf aabb _ _) =
    aabbIntersect val aabb
{-# INLINE checkOctant #-}

-- BVH --

data BVH s a =
    Node !Axis !(AABB s) (BVH s a) (BVH s a)
  | Leaf !Axis !(AABB s) {-# UNPACK #-} !Int [a]
  deriving (Show, Eq, Functor)

instance (RayTrace s a, Ord s, Fractional s) => RayTrace s (BVH s a) where
    rayTrace _ (Leaf _ _ 0 _) = Miss
    rayTrace ray (Leaf _ aabb _ xs) =
        if isHit $ rayTrace ray aabb
            then
                let casts = map (rayTrace ray) xs
                    hits = filter isHit casts
                in if null hits
                    then Miss
                    else head hits -- TODO: Choose the correct hit
                                   -- (The one closest to ray origin.)
        else Miss
    rayTrace ray@(Ray _ direction) (Node axis aabb left right) =
        if isHit $ rayTrace ray aabb
            then if isHit firstInt
                    then firstInt
                else otherInt
        else Miss
      where
        (childa, childb) =
            if component axis direction > 0
                then (left, right) else (right, left)
        firstInt = rayTrace ray childa
        otherInt = firstInt `seq` rayTrace ray childb

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
    ((left, right), allBounds) = splitMidpoint xs dim
    dim = maximumExtent . centroidBounds $ xs

intersectBVH :: (RayTrace s a, Ord s, Fractional s) =>
                 Ray s -> BVH s a -> Bool
intersectBVH _ (Leaf _ _ 0 _) = False
intersectBVH ray (Leaf _ aabb _ xs) =
    if isHit $ rayTrace ray aabb
        then any (isHit . rayTrace ray) xs
    else False
intersectBVH ray@(Ray _ direction) (Node axis aabb left right) =
    if isHit $ rayTrace ray aabb
        then firstInt || otherInt
    else False
  where
    (childa, childb) =
        if component axis direction > 0
            then (left,right) else (right,left)
    firstInt = intersectBVH ray childa
    otherInt = intersectBVH ray childb

nearby :: (Ord s, Fractional s) => Ray s -> BVH s a -> [a]
nearby ray = D.toList . nearbyD ray

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

centroidBounds :: (HasAABB s a, Fractional s, Ord s) => [a] -> AABB s
centroidBounds = foldr expandAABBToFit emptyAABB . map boundingBox

splitMidpoint :: (HasAABB s a, Fractional s, Ord s) =>
    [a] -> Axis -> (([a],[a]), AABB s)
splitMidpoint xs axis =
    let (ls,rs) = partition isLeft xs
    in if null ls && not (null rs)
            then (splitMiddle rs, allBounds)
        else ((ls,rs), allBounds)
  where
    splitMiddle ys = splitAt (length ys `div` 2) ys
    isLeft x = component axis (aabbCentroid $ boundingBox x) < midpoint
    midpoint = component axis $ aabbCentroid allBounds
    allBounds = foldr expandAABBToFit emptyAABB xs
