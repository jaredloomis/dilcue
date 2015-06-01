module Coherence where

import Ray
import Intersect

data RayPacket = RayPacket
    {-# UNPACK #-} !Ray
    {-# UNPACK #-} !Ray
    {-# UNPACK #-} !Ray
    {-# UNPACK #-} !Ray
  deriving (Show, Eq)

data PacketResult = PacketResult !Rayint !Rayint !Rayint !Rayint
  deriving (Show, Eq)
