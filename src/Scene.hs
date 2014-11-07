{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Scene where

import Intersect
import AABB
import Shape
import RayTrace
import Accel
import Coherence

data Solid s = forall a. (RayTrace s a, HasAABB s a) => Solid a

instance RayTrace s (Solid s) where
    rayTrace ray (Solid s) = rayTrace ray s
instance HasAABB s (Solid s) where
    boundingBox (Solid s) = boundingBox s

data DilcueScene s =
    DilcueScene
        (LinearBVH s (Solid s))
        (LinearBVH s (Warp s))

instance (Fractional s, Ord s) => RayTrace s (DilcueScene s) where
    rayTrace ray (DilcueScene solids warps) =
        case rayTrace ray warps of
            Change ray' -> rayTrace ray' solids
            _           -> rayTrace ray  solids
    rayTracePacket rays@(RayPacket r1 r2 r3 r4) (DilcueScene solids warps) =
        case rayTracePacket rays warps of
            PacketResult Miss Miss Miss Miss ->
                rayTracePacket rays solids
            result -> rayTracePacket (toPack result) solids
      where
        toPack (PacketResult a b c d) =
            RayPacket
                (toPack' a r1)
                (toPack' b r2)
                (toPack' c r3)
                (toPack' d r4)

        toPack' (Change r') _ = r'
        toPack' _           r = r
