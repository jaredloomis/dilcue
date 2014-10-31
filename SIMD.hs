module SIMD where

import Data.SIMD.SIMD4

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V

distance_simd4 :: UV.Vector Float -> UV.Vector Float -> Float
distance_simd4 v1 v2 = sqrt $ plusHorizontalX4 $ go 0 (UV.length v1'-1)
    where
        v1' = unsafeVectorizeUnboxedX4 v1
        v2' = unsafeVectorizeUnboxedX4 v2

        go tot (-1) = tot
        go tot i = go tot' (i-1)
            where
                tot' = tot+diff*diff
                diff = v1' `UV.unsafeIndex` i - v2' `UV.unsafeIndex` i
