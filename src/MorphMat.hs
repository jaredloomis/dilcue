module MorphMat where

import Data.Vec ((:.)(..), Vec3)
--import qualified Data.Vec as V

type MorphMat = Vec3 Float -> Vec3 Float

translation :: Vec3 Float -> MorphMat
translation = (+)

rotationX :: Float -> MorphMat
rotationX r (x:.y:.z:.()) =
    let cr = cos r
        sr = sin r
    in x :.
       ((cr * y) + ((-sr) * z)) :.
       ((sr * y) + (cr * z)) :.
       ()

rotationY :: Float -> MorphMat
rotationY r (x:.y:.z:.()) =
    let cr = cos r
        sr = sin r
    in ((cr * x) + (sr * z)) :.
       y :.
       (((-sr) * x) + (cr * z)) :.
       ()

rotationZ :: Float -> MorphMat
rotationZ r (x:.y:.z:.()) =
    let cr = cos r
        sr = sin r
    in ((cr * x) + ((-sr) * y)) :.
       ((sr * x) + (cr * y)) :.
       z :.
       ()

rotationEuler :: Vec3 Float -> MorphMat
rotationEuler (x:.y:.z:.()) =
    rotationZ z . rotationY y . rotationX x
