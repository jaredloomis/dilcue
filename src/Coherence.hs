module Coherence where

import Ray
import Intersect

data RayPacket s = RayPacket !(Ray s) !(Ray s) !(Ray s) !(Ray s)
  deriving (Show, Eq)

data PacketResult s =
    PacketResult !(Rayint s) !(Rayint s) !(Rayint s) !(Rayint s)
  deriving (Show, Eq)
