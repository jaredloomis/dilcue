{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Render where

import Control.Parallel.Strategies hiding (dot)
import Data.Vec ((:.)(..), Vec3, Mat44)
import qualified Data.Vec as Vec
import Control.Applicative
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import Control.Monad.ST (ST, runST)
import Control.Loop (numLoop)
import Foreign.ForeignPtr.Safe
import Debug.Trace (trace)

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RF

import qualified Codec.Picture as J
import qualified Graphics.Rendering.OpenGL.GL as GL

import BVH
import Ray
import Shape
import Intersect

data View s = View {
    viewOrigin :: Vec3 s,
    viewMatrix :: Mat44 s,
    viewDimensions :: (Int, Int),
    viewResolution :: s,
    viewNear :: s
    }

{-
type RawImg = R.Array R.U (R.Z R.:. Int) Word8
data Image' = Image' !Int !Int RawImg

rayTraceImg' :: (Show a, Ord a, Floating a) =>
    View a -> BVH a (Triangle a) -> Image'
rayTraceImg' (View (Ray origin direction) (w,h) res near) tris =
    Image' w h (R.fromUnboxed (R.Z R.:. vLen) createImage)
  where
    vLen = (w*4)*h

    createImage :: VU.Vector Word8
    createImage = runST $ do
        mutArr <- VUM.unsafeNew vLen
        numLoop 0 ((vLen `div` 4) - 1) $ \i -> do
            let i4 = 4*i
                Color r g b a = getIndex i
            VUM.unsafeWrite mutArr i4 r
            VUM.unsafeWrite mutArr (i4+1) g
            VUM.unsafeWrite mutArr (i4+2) b
            VUM.write mutArr (i4+3) a
        VU.unsafeFreeze mutArr

    getIndex :: Int -> Color
    getIndex i =
            let x = (*res) $ fromIntegral $ (i `mod` w) - (w `div` 2)
                y = (*res) $ fromIntegral $ ((-i) `div` w) - (h `div` 2)
                z = near
                pixel = (x:.y:.z:.())
                ray = segmentRay origin $ pixel
            in getColor $ rayTrace ray tris

    getColor Miss = Color 0 0 0 1
    getColor hit@(Hit{}) = riColor hit

extractImg' :: Image' -> IO (Int, Int, Ptr Word8)
extractImg' (Image' w h dat) = do
    let del = R.delay dat
    fptr <- mallocForeignPtrBytes (w*4 * h)
    RF.computeIntoP fptr del
    withForeignPtr fptr $ \ptr -> return (w,h,ptr)

--extractImgRaw :: Image Color -> IO (Int, Int, Ptr Word8)
--extractImg (Image )

imgToTexObj' :: Image' -> IO GL.TextureObject
imgToTexObj' img = do
    (w,h,ptr) <- extractImg' img
    texObject <- GL.genObjectName
    GL.textureBinding GL.Texture2D GL.$= Just texObject

    GL.texImage2D GL.Texture2D
        -- No proxy.
        GL.NoProxy
        -- Mipmaps.
        0
        -- Use RGBA format.
        GL.RGBA'
        -- Size of image.
        (GL.TextureSize2D (fromIntegral w) $ fromIntegral h)
        -- No borders
        0
        -- The pixel data.
        (GL.PixelData GL.RGBA GL.UnsignedByte ptr)

    GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Mirrored, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Mirrored, GL.ClampToEdge)

    return texObject
-}

{-
-- Storable Vector Implementation

data Image = Image {-# UNPACK #-} !Int {-# UNPACK #-} !Int
                   (VS.Vector Word8) deriving (Show, Eq)

parVectChunk :: Int -> Strategy a -> Strategy (V.Vector a)
parVectChunk n strat xs
    | n <= 1    = parTraversable strat xs
    | otherwise =
        let chunks = vchunk n xs
        in V.concat `fmap` parList (evalTraversable strat) chunks

vchunk :: Int -> V.Vector a -> [V.Vector a]
vchunk n xs
    | V.null xs = []
    | otherwise =
        let (as,bs) = V.splitAt n xs
        in as : vchunk n bs

parVectChunk' :: VS.Storable a => Int -> Strategy a -> Strategy (VS.Vector a)
parVectChunk' n strat xs =
    let chunks = vchunk' n xs
    in VS.concat `fmap` parList (VS.mapM strat) chunks

vchunk' :: VS.Storable a => Int -> VS.Vector a -> [VS.Vector a]
vchunk' n xs
    | VS.null xs = []
    | otherwise =
        let (as,bs) = VS.splitAt n xs
        in as : vchunk' n bs

rayTraceImg :: (Show a, Ord a, Floating a) =>
    View a -> BVH a (Triangle a) -> Image
rayTraceImg (View (Ray origin direction) (w,h) res near) tris =
    Image w h (createImage `using`
                parVectChunk' (vLen `div` 4) rseq)
  where
    vLen = (w*4)*h

    createImage :: VS.Vector Word8
    createImage = runST $ do
        mutArr <- VSM.unsafeNew vLen
        numLoop 0 ((vLen `div` 4) - 1) $ \i -> do
            let i4 = 4*i
                Color r g b a = getIndex i
            VSM.unsafeWrite mutArr i4 r
            VSM.unsafeWrite mutArr (i4+1) g
            VSM.unsafeWrite mutArr (i4+2) b
            VSM.write mutArr (i4+3) a
        VS.unsafeFreeze mutArr

    getIndex :: Int -> Color
    getIndex i =
            let x = (*res) $ fromIntegral $ (i `mod` w) - (w `div` 2)
                y = (*res) $ fromIntegral $ ((-i) `div` w) - (h `div` 2)
                z = near
                pixel = (x:.y:.z:.())
                ray = segmentRay origin $ pixel
            in getColor $ rayTrace ray tris

    getColor Miss = Color 0 0 0 1
    getColor hit@(Hit{}) = riColor hit

extractImg :: Image -> IO (Int, Int, Ptr Word8)
extractImg (Image w h dat) =
    VS.unsafeWith dat $ \ptr -> return (w,h,ptr)

--extractImgRaw :: Image Color -> IO (Int, Int, Ptr Word8)
--extractImg (Image )

imgToTexObj :: Image -> IO GL.TextureObject
imgToTexObj img = do
    (w,h,ptr) <- extractImg img
    texObject <- GL.genObjectName
    GL.textureBinding GL.Texture2D GL.$= Just texObject

    GL.texImage2D GL.Texture2D
        -- No proxy.
        GL.NoProxy
        -- Mipmaps.
        0
        -- Use RGBA format.
        GL.RGBA'
        -- Size of image.
        (GL.TextureSize2D (fromIntegral w) $ fromIntegral h)
        -- No borders
        0
        -- The pixel data.
        (GL.PixelData GL.RGBA GL.UnsignedByte ptr)

    GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Mirrored, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Mirrored, GL.ClampToEdge)

    return texObject
-}

data Image = Image !Int !Int (V.Vector Word8) deriving (Show, Eq)

parVectChunk :: Int -> Strategy a -> Strategy (V.Vector a)
parVectChunk n strat xs
    | n <= 1    = parTraversable strat xs
    | otherwise =
        let chunks = vchunk n xs
        in V.concat `fmap` parList (evalTraversable strat) chunks

vchunk :: Int -> V.Vector a -> [V.Vector a]
vchunk n xs
    | V.null xs = []
    | otherwise =
        let (as,bs) = V.splitAt n xs
        in as : vchunk n bs

rayTraceImg' :: (Show a, Ord a, Floating a) =>
    View a -> LinearBVH a (Triangle a) -> Image
rayTraceImg' (View origin matrix (w,h) res near) tris =
    trace (show $ getRay 10 10) $
    Image w h (createImage `using`
                parVectChunk (vLen `div` 4) rseq)
  where
    vLen = w*4*h

    createImage :: V.Vector Word8
    createImage = runST $ do
        mutArr <- VM.unsafeNew vLen
        numLoop 0 ((vLen `div` 4) - 1) $ \i -> do
            let i4 = 4*i
                (x,y) = iToXY i
                Color r g b a = getIndex x y
            VM.unsafeWrite mutArr i4 r
            VM.unsafeWrite mutArr (i4+1) g
            VM.unsafeWrite mutArr (i4+2) b
            VM.unsafeWrite mutArr (i4+3) a
        V.unsafeFreeze mutArr

    getRay x y =
        let z     = near
            pixel = x:.y:.z:.()
            vec   = pixel
            dir4  = matrix `Vec.multmv` (vec `Vec.snoc` 1)
            dir   = Vec.take Vec.n3 dir4
        in Ray origin (Vec.normalize dir)

    getIndex x y =
        let ray = getRay x y
        in getColor $ rayTrace ray tris
    {-# INLINE getIndex #-}

    getColor Miss = Color 0 0 0 255
    getColor hit@(Hit{}) = riColor hit

    iToXY i = (res * fromIntegral ((i `mod` w) - (w `div` 2)),
               res * fromIntegral (((-i) `div` w) + (h `div` 2)))

rayTraceImg :: (Show a, Ord a, Floating a) =>
    View a -> BVH a (Triangle a) -> Image
rayTraceImg (View origin matrix (w,h) res near) tris =
    trace (show $ getRay 10 10) $
    Image w h (createImage `using`
                parVectChunk (vLen `div` 4) rseq)
  where
    vLen = w*4*h

    createImage :: V.Vector Word8
    createImage = runST $ do
        mutArr <- VM.unsafeNew vLen
        numLoop 0 ((vLen `div` 4) - 1) $ \i -> do
            let i4 = 4*i
                (x,y) = iToXY i
                Color r g b a = getIndex x y
            VM.unsafeWrite mutArr i4 r
            VM.unsafeWrite mutArr (i4+1) g
            VM.unsafeWrite mutArr (i4+2) b
            VM.unsafeWrite mutArr (i4+3) a
        V.unsafeFreeze mutArr
{-
    getRay x y =
        let aspect = fromIntegral w / fromIntegral h
            bz = 0.00001
            fov = pi * (45 / 360)
            (tx,ty) = (aspect * bz * tan fov, bz * tan fov)
            w' = Vec.normalize direction
            u = Vec.cross (0:.1:.0:.()) w'
            v = Vec.cross w' u
            dirx = (-tx) + (2*tx) * (x / fromIntegral w)
            diry = (-ty) + (2*ty) * (y / fromIntegral h)
            uprime = Vec.map (*dirx) u
            vprime = Vec.map (*diry) v
            wprime = Vec.map (*bz) w'
        in Ray origin (Vec.normalize $ uprime + vprime + wprime)
-}
    getRay x y =
        let z     = near
            pixel = x:.y:.z:.()
            vec   = pixel
            dir4  = matrix `Vec.multmv` (vec `Vec.snoc` 1)
            dir   = Vec.take Vec.n3 dir4
        in Ray origin (Vec.normalize dir)

    getIndex x y = --getColor $ rayTrace (getRay x y) tris
        let ray = getRay x y
        in getColor $ rayTrace ray tris
    {-# INLINE getIndex #-}

    getColor Miss = Color 0 0 0 255
    getColor hit@(Hit{}) = riColor hit

    iToXY i = (res * fromIntegral ((i `mod` w) - (w `div` 2)),
               res * fromIntegral (((-i) `div` w) + (h `div` 2)))
{-
rayTraceImgOct :: (Show a, Ord a, Floating a) =>
    View a -> Octree a (Triangle a) -> Image
rayTraceImgOct (View (Ray origin direction) (w,h) res near) tris =
    Image w (createImage `using`
                parVectChunk (vLen `div` 4) rseq)
  where
    vLen = w*h

    createImage =
        V.generate vLen $ \i ->
            let x = (*res) $ fromIntegral $ (i `mod` w) - (w `div` 2)
                y = (*res) $ fromIntegral $ ((-i) `div` w) - (h `div` 2)
                z = near
                pixel = (x:.y:.z:.())
                ray = segmentRay origin $ pixel
            in getColor $ rayTrace ray tris
    getColor Miss = Color 0 0 0 1
    getColor hit@(Hit{}) = riColor hit
-}
extractImg :: Image -> IO (Int, Int, Ptr Word8)
extractImg (Image w h dat) =
    let storableDat = VS.convert dat
    in VS.unsafeWith storableDat $ \ptr -> return (w,h,ptr)

--extractImgRaw :: Image Color -> IO (Int, Int, Ptr Word8)
--extractImg (Image )

imgToTexObj :: Image -> IO GL.TextureObject
imgToTexObj img = do
    (w,h,ptr) <- extractImg img
    texObject <- GL.genObjectName
    GL.textureBinding GL.Texture2D GL.$= Just texObject

    GL.texImage2D GL.Texture2D
        -- No proxy.
        GL.NoProxy
        -- Mipmaps.
        0
        -- Use RGBA format.
        GL.RGBA'
        -- Size of image.
        (GL.TextureSize2D (fromIntegral w) $ fromIntegral h)
        -- No borders
        0
        -- The pixel data.
        (GL.PixelData GL.RGBA GL.UnsignedByte ptr)

    GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Mirrored, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Mirrored, GL.ClampToEdge)

    return texObject
