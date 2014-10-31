{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Dilcue where

import Data.Vec ((:.)(..), Vec3)

import Shape

data Elgnairt s = Elgnairt (Triangle s)

class Dilcue s a | a -> s where
    reifyDilcue :: a -> Vec3 s -> [Triangle s]
