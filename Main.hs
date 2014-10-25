{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
module Main where

import Data.Vec as Vec hiding (map, length)
import Data.IORef
import Debug.Trace (trace)

import qualified Codec.Picture as J

import qualified Graphics.Rendering.OpenGL.GL as GL

import Language.GLSL.Monad

import Ray
--import Intersect
import BVH
import Shape
import Render
import AABB hiding (X,Y,Z)

myBVH :: BVH Float (Triangle Float)
myBVH = mkBVH triangles

myOctree :: Octree Float (Triangle Float)
myOctree = mkOctree (AABB (-1000) 1000) triangles

triangles'' :: Float -> [Triangle Float]
triangles'' y = (flip map) [(-100)..100] $ \x ->
    let sx = sin x * 50
        ty = tan y * 50
        z = cos (x/y) * 10
    in Triangle (sx:.ty:.z:.())
                ((sx+1):.ty:.z:.())
                ((sx+0.5):.(ty+1):.z:.())
       

triangles' :: Float -> [Triangle Float]
triangles' y = (flip fmap) [(-10)..10] $ \x ->
                Triangle (x    :.y:.1:.())
                         ((x+1):.y:.1:.())
                         ((x+0.5):.(y+1):.1:.())

triangles :: [Triangle Float]
triangles = --concatMap triangles'  [(-10)..10] ++
            concatMap triangles'' [(-10)..10]

vert :: ShaderM GL.VertexShader a ()
vert = do
    version "430 core"

    position <- layoutIn ["location=0"] vec3 ("position",
                (const screenBufferData))

    _ <- out vec2 "tcoord" $= --position .@ X .& Y
        (fltd 0 +.+ fltd 1) .-
        (((position .@ X .& Y) .+ (fltd 1 +.+ fltd 1)) ./ fltd 2.0)

    glPosition #= position +.+ fltd 1.0

frag :: ShaderM GL.FragmentShader GL.TextureObject ()
frag = do
    version "430 core"
    tcoord <- inn vec2 "tcoord"

    img <- uniform sampler2D
        ("img", \to -> Sampler2DInfo to $ GL.TextureUnit 0)

    color <- out vec4 "color"
    color #= texture img tcoord

glProg :: GL.TextureObject ->
          IO (ShaderProgram GL.TextureObject)
glProg img =
    let shSeq = Shader Proxy vert -&> lastly (Shader Proxy frag)
    in createProgram shSeq img

myCam :: View Float
myCam =
    let ray = Ray (0:.0:.(-20):.()) (1:.0:.0:.())
    in View ray (100, 100) 0.01 0.5

startGL :: BVH Float (Triangle Float) -> IO ()
startGL bvh = do
    let img = rayTraceImg myCam bvh
    win <- openWindow
    initGL win
    count <- newIORef (0::Float)
    to <- imgToTexObj img
    prog <- glProg to
    let galaxy = MonadicGalaxy prog (updateImage count) to
    mainLoop win $ [galaxy] -|> []
  where
    updateImage :: IORef Float -> GL.TextureObject -> IO GL.TextureObject
    updateImage count _ = do
        i <- readIORef count
        let img = rayTraceImg myCam{
                viewRay = Ray (i:.0:.0:.()) 0} bvh
        writeIORef count $ i+0.05
        imgToTexObj img

main :: IO ()
main = do
    let img = rayTraceImg myCam myBVH
        jimg = toJuicy img
    J.writePng "rayTrace.png" jimg
    startGL myBVH
    --prettyImgR $ rayTraceImgP myBVH
    --prettyImg $ rayTraceImg myBVH
    --putStrLn . pretty $ rayTraceSceneBVH myBVH
