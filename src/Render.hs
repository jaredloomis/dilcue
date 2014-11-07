{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Render where

import Data.Vec ((:.)(..), Vec3, Mat44)
import qualified Data.Vec as Vec
import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr (withForeignPtr)

import Control.Loop (forLoop)

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI

import qualified Graphics.Rendering.OpenGL.GL as GL

import Ray
import RayTrace
import Intersect
import Coherence

data View s = View {
    viewOrigin :: !(Vec3 s),
    viewMatrix :: !(Mat44 s),
    viewDimensions :: {-# UNPACK #-} !(Int, Int),
    viewResolution :: !s,
    viewNear :: !s
    } deriving (Show, Eq)

data Image = Image
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !B.ByteString

-------------------------------------------
-- ByteString version with ray coherence --
-------------------------------------------

rayTraceImgCoh :: (RayTrace s a, Ord s, Floating s) =>
    View s -> a -> Image
rayTraceImgCoh (View origin mat (w,h) res near) tris =
    Image w h createImage
  where
    vLen :: Int
    vLen = w*h*4

    createImage :: B.ByteString
    createImage = BI.unsafeCreate vLen $ \ptr ->
        forLoop 0 (<w) (+2) $ \x ->
            forLoop 0 (<h) (+2) $ \y ->
                let packet = getPacket x y
                    PacketResult h1 h2 h3 h4 = rayTracePacket packet tris
                    (# c1,c2,c3,c4 #) =
                        (# getColor h1, getColor h2,
                           getColor h3, getColor h4 #)
                    (# i1,i2,i3,i4 #) =
                        (# xyToCI x y,     xyToCI (x+1) y,
                           xyToCI x (y+1), xyToCI (x+1) (y+1) #)
                in do
                    writeColor i1 c1 ptr
                    writeColor i2 c2 ptr
                    writeColor i3 c3 ptr
                    writeColor i4 c4 ptr
      where
        writeColor i (Color r g b a) ptr
            | i + 3 > vLen = return ()
            | otherwise = do
                pokeByteOff ptr  i    r
                pokeByteOff ptr (i+1) g
                pokeByteOff ptr (i+2) b
                pokeByteOff ptr (i+3) a

    getPacket x y =
        let r1 = getRay x y
            r2 = getRay (x+1) y
            r3 = getRay x (y+1)
            r4 = getRay (x+1) (y+1)
        in RayPacket r1 r2 r3 r4

    getRay x y =
        let x'    = fromIntegral $ x - (w `div` 2)
            y'    = fromIntegral $ y - (h `div` 2)
            z     = near
            pixel = (res*x'):.
                    (res*y'):.
                    z:.()
            dir4  = mat `Vec.multmv` (pixel `Vec.snoc` 0)
            dir   = Vec.take Vec.n3 dir4
        in Ray origin dir

    getColor Miss = Color 0 0 0 255
    getColor hit@(Hit{}) = riColor hit
    getColor (Change newRay) = getColor $ rayTrace newRay tris

    xyToI :: Int -> Int -> Int
    xyToI x y = (y*w) + x

    xyToCI :: Int -> Int -> Int
    xyToCI x y = xyToI x y * 4

imgToTexObj :: Image -> IO GL.TextureObject
imgToTexObj (Image w h (BI.PS fptr _ _)) = do
    texObject <- GL.genObjectName
    GL.textureBinding GL.Texture2D GL.$= Just texObject

    withForeignPtr fptr $ \ptr ->
        GL.texImage2D GL.Texture2D
            -- No proxy.
            GL.NoProxy
            -- Mipmaps.
            0
            -- Use RGBA format.
            GL.RGBA'
            -- Size of image.
            (GL.TextureSize2D (fromIntegral w) (fromIntegral h))
            -- No borders
            0
            -- The pixel data.
            (GL.PixelData GL.RGBA GL.UnsignedByte ptr)

    GL.textureFilter GL.Texture2D GL.$= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Mirrored, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Mirrored, GL.ClampToEdge)

    return texObject

-----------------------------------
-- Parallel Data.Vector version. --
-----------------------------------
{-
data View s = View {
    viewOrigin :: Vec3 s,
    viewMatrix :: Mat44 s,
    viewDimensions :: {-# UNPACK #-} !(Int, Int),
    viewResolution :: !s,
    viewNear :: !s
    } deriving (Show, Eq)

data Image = Image {-# UNPACK #-} !Int {-# UNPACK #-} !Int
                   {-# UNPACK #-} !(V.Vector Word8) deriving (Show, Eq)

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

rayTraceImg :: (RayTrace s a, Ord s, Floating s) =>
    View s -> a -> Image
rayTraceImg (View origin matrix (w,h) res near) tris =
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

extractImg :: Image -> IO (Int, Int, Ptr Word8)
extractImg (Image w h dat) =
    let storableDat = VS.convert dat
    in VS.unsafeWith storableDat $ \ptr -> return (w,h,ptr)

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
