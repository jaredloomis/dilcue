{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
module RayTrace where

import Data.Vec ((:.)(..))
import qualified Data.Vec as V

import Ray
import Intersect
import Coherence
import Shape
import AABB

class RayTrace a where
    rayTrace :: Ray -> a -> Rayint

    rayTracePacket :: RayPacket -> a -> PacketResult
    rayTracePacket (RayPacket a b c d) x =
        PacketResult (rayTrace a x)
                     (rayTrace b x)
                     (rayTrace c x)
                     (rayTrace d x)
    {-# MINIMAL rayTrace #-}


rayTraceTriangle :: Ray -> Triangle -> Rayint
rayTraceTriangle (Ray origin dir) (Triangle p1 p2 p3) =
    let e1 = p2 - p1
        e2 = p3 - p1
        s1 = V.cross dir e2
        divisor = V.dot s1 e1
    in guard (divisor == 0) $
        let invdivisor = 1.0 / divisor
            d = origin - p1
            b1 = V.dot d s1 * invdivisor
        in guard (b1 < 0 || b1 > 1) $
            let s2 = V.cross d e1
                b2 = V.dot dir s2 * invdivisor
            in guard (b2 < 0 || b1 + b2 > 1) $
                let t = V.dot e2 s2 * invdivisor
                in guard (t < 0) $
                    Hit t (origin + V.map (*t) dir)
                        (V.normalize $ V.cross e1 e2)
                        (Color 255 255 255 255)
  where
    guard False e = e
    guard True _  = Miss
    {-# INLINE guard #-}

instance RayTrace Triangle where
    rayTrace = rayTraceTriangle
    {-# INLINE rayTrace #-}

rayTraceAABB :: Ray -> AABB -> Rayint
rayTraceAABB (Ray (ox:.oy:.oz:.()) (dx:.dy:.dz:.()))
             (AABB (lx:.ly:.lz:.()) (hx:.hy:.hz:.()))
    | lastin > firstout || firstout < 0 = Miss
    | lastin < 0 =
        let n = case firstaxis of
                X -> if dx <= 0 then 1:.0:.0:.()
                                else (-1):.0:.0:.()
                Y -> if dy <= 0 then 0:.1:.0:.()
                                else 0:.(-1):.0:.()
                Z -> if dz <= 0 then 0:.0:.1:.()
                                else 0:.0:.(-1):.()
        in Hit firstout
            ((ox+dx*lastin):.(oy+dy*lastin):.(oz+dz*lastin):.())
            n (Color 255 255 255 255)
    | otherwise =
        let n = case lastaxis of
                X -> if dx <= 0 then 1:.0:.0:.()
                                else (-1):.0:.0:.()
                Y -> if dy <= 0 then 0:.1:.0:.()
                                else 0:.(-1):.0:.()
                Z -> if dz <= 0 then 0:.0:.1:.()
                                else 0:.0:.(-1):.()
        in Hit lastin
            ((ox+dx*lastin):.(oy+dy*lastin):.(oz+dz*lastin):.())
            n (Color 255 255 255 255)
  where
    (# !inx, !outx #) =
        if dx > 0
            then (# (lx-ox)/dx, (hx-ox)/dx #)
        else (# (hx-ox)/dx, (lx-ox)/dx #)
    (# !iny, !outy #) =
        if dy > 0
            then (# (ly-oy)/dy, (hy-oy)/dy #)
        else (# (hy-oy)/dy, (ly-oy)/dy #)
    (# !inz, !outz #) =
        if dz > 0
            then (# (lz-oz)/dz, (hz-oz)/dz #)
        else (# (hz-oz)/dz, (lz-oz)/dz #)
    (# !lastaxis, !lastin #)
        | iny > inz =
            if inx > iny then (# X, inx #)
                         else (# Y, iny #)
        | otherwise =
            if inx > inz then (# X, inx #)
                         else (# Z, inz #)
    (# !firstaxis, !firstout #)
        | outy < outz =
            if outx < outy then (# X, outx #)
                           else (# Y, outy #)
        | otherwise   =
            if outx < outz then (# X, outx #)
                           else (# Z, outz #)

instance RayTrace AABB where
    rayTrace = rayTraceAABB

instance RayTrace Warp where
    rayTrace ray (Warp aabb warp) =
        let aabbTrace = rayTrace ray aabb
        in if isHit aabbTrace
            then
                let ray' = warp ray
                in  Change ray'
            else Miss
