{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Render where

import Control.Parallel.Strategies hiding (dot)
import Data.Vec ((:.)(..), Vec3)
import qualified Data.Vec as Vec
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import Debug.Trace (trace)

import qualified Codec.Picture as J
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

import qualified Graphics.Rendering.OpenGL.GL as GL

import BVH
import Ray
import Shape
import Intersect

data View a = View {
    viewRay :: Ray a,
    viewDimensions :: (Int, Int),
    viewResolution :: a,
    viewNear :: a
    }

data Image a = Image !Int (V.Vector a) deriving (Show, Eq)

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

rayTraceImg :: (Show a, Ord a, Floating a) =>
    View a -> BVH a (Triangle a) -> Image Color
rayTraceImg (View (Ray origin direction) (w,h) res near) tris =
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

rayTraceImgOct :: (Show a, Ord a, Floating a) =>
    View a -> Octree a (Triangle a) -> Image Color
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

toJuicy :: Image Color -> J.Image J.PixelRGBA8
toJuicy (Image w xs) =
    let pixF x y =
            let (Color r g b a) = xs V.! ((y*w) + x)
            in J.PixelRGBA8 r g b a
    in J.generateImage pixF w (V.length xs `div` w)

extractImg :: Image Color -> IO (Int, Int, Ptr Word8)
extractImg img =
    let J.Image w h dat = toJuicy img
    in VS.unsafeWith dat $ \ptr -> return (w,h,ptr)

--extractImgRaw :: Image Color -> IO (Int, Int, Ptr Word8)
--extractImg (Image )

imgToTexObj :: Image Color -> IO GL.TextureObject
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
