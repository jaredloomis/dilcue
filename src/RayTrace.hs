{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
module RayTrace where

import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)

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

{-
foreign import ccall unsafe "rayTraceTriangleC" rayTraceTriangleC ::
    Float -> Float -> Float ->
    Float -> Float -> Float ->
    Float -> Float -> Float ->
    Float -> Float -> Float ->
    Float -> Float -> Float ->
    Ptr Float -> Ptr Float -> Ptr Float ->
    Int

rayTraceTriangle :: Ray -> Triangle -> Rayint
rayTraceTriangle (Ray (ox:.oy:.oz:.()) (dx:.dy:.dz:.()))
                   (Triangle (x1:.y1:.z1:.())
                             (x2:.y2:.z2:.())
                             (x3:.y3:.z3:.())) = unsafePerformIO $
    alloca $ \t -> alloca $ \u -> alloca $ \v -> do
        let hit = rayTraceTriangleC ox oy oz dx dy dz
                          x1 y1 z1 x2 y2 z2 x3 y3 z3
                          t u v
        case hit of
            0 -> return Miss
            _ -> do
                tx <- peek t
                ux <- peek u
                vx <- peek v
                return $ Hit 10 (x1:.y1:.z1:.()) (tx:.ux:.vx:.())
                                (Color 255 255 255 255)
-}

rayTraceTriangle :: Ray -> Triangle -> Rayint
rayTraceTriangle (Ray origin dir) (Triangle p1 p2 p3) =
    let edge1 = p2 - p1
        edge2 = p3 - p1
        pvec = V.cross dir edge2
        det = V.dot pvec edge1
    in guard (det == 0) $
        let invdet = 1.0 / det
            tvec = origin - p1
            b1 = V.dot tvec pvec * invdet
        in guard (b1 < 0 || b1 > 1) $
            let s2 = V.cross tvec edge1
                b2 = V.dot dir s2 * invdet
            in guard (b2 < 0 || b1 + b2 > 1) $
                let t = V.dot edge2 s2 * invdet
                in guard (t < 0) $
                    Hit t (origin + V.map (*t) dir)
                        (V.normalize $ V.cross edge1 edge2)
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
