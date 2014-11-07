{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Vec as Vec ((:.)(..), Vec3)
import qualified Data.Vec as Vec
import Data.IORef
import Debug.Trace (trace)
import System.Exit (exitSuccess)
import Data.Time.Clock

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW

import Language.GLSL.Monad

import Ray
import Scene
import Intersect
import Accel
import Shape
import Render
import AABB hiding (X,Y,Z)
--import SIMD

myBVH :: BVH Float (Solid Float)
myBVH = mkBVH triangles

warpBVH :: BVH Float (Warp Float)
warpBVH = mkBVH [Warp (AABB 0 10) warpSpace]

warpSpace :: Ray Float -> Ray Float
warpSpace ray@(Ray origin@(ox:.oy:.oz:.()) dir)
    | pointInAABB origin (AABB 0 10) =
        let m = 1 + ox
            mats = Vec.scaling (1:.m:.m:.())
            multmv3 mt v = Vec.take Vec.n3 (mt `Vec.multmv` (v `Vec.snoc` 1))
        in Ray origin $ multmv3 mats dir
    | otherwise = ray

myWarpF :: Ray Float -> Ray Float
myWarpF (Ray origin dir) =
    let delta = 3 * sin (Vec.get Vec.n0 dir) * cos (Vec.get Vec.n2 origin)
        rotMat = Vec.rotationX delta `Vec.multmm` Vec.rotationY (cos delta)
        dir4 = dir `Vec.snoc` 1
        dir4' = rotMat `Vec.multmv` dir4
        dir' = Vec.take Vec.n3 dir4'
    in Ray origin dir'

scene :: DilcueScene Float
scene = DilcueScene (flatten myBVH) (flatten warpBVH)

--myOctree :: Octree Float (Solid Float)
--myOctree = mkOctree (AABB (-1000) 1000) triangles

trianglesSimple :: Float -> [Solid Float]
trianglesSimple y = (flip map) [(-10)..10] $ \x ->
    Solid $
    Triangle (x:.y:.1:.())
             ((x+1):.y:.1:.())
             ((x+0.5):.(y+1):.1:.())

triangles'' :: Float -> [Solid Float]
triangles'' y = (flip map) [(-10)..10] $ \x ->
    let sx = sin x * 50
        ty = tan y * 50
        z  = cos (x/y * 20) * 10
    in Solid $ Triangle (sx:.ty:.z:.())
                ((sx+1):.ty:.z:.())
                ((sx+0.5):.(ty+1):.z:.())

triangles' :: Float -> [Solid Float]
triangles' y = (flip fmap) [(-10)..10] $ \x ->
                Solid $
                Triangle (x    :.y:.1:.())
                         ((x+1):.y:.1:.())
                         ((x+0.5):.(y+1):.1:.())

triangles :: [Solid Float]
triangles = --concatMap triangles'  [(-10)..10] ++
            --concatMap triangles'' [(-20)..20]
            concatMap trianglesSimple [(-10)..10]

vert :: ShaderM GL.VertexShader a ()
vert = do
    version "430 core"

    position <- layoutIn ["location=0"] vec3 ("position",
                (const screenBufferData))

    _ <- out vec2 "tcoord" $=
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
    View 0 Vec.identity (300, 300) 0.01 1

isKeyDown :: GLFW.Window -> GLFW.Key -> IO Bool
isKeyDown win k = do
    kp <- GLFW.getKey win k
    return (kp == GLFW.KeyState'Repeating || kp == GLFW.KeyState'Pressed)

rawToMovement :: Vec3 Float -> Vec3 Float -> Vec3 Float
rawToMovement pos rot = undefined

camMovement :: GLFW.Window -> IO (Vec3 Float)
camMovement win = do
    w <- isKeyDown win GLFW.Key'W
    a <- isKeyDown win GLFW.Key'A
    s <- isKeyDown win GLFW.Key'S
    d <- isKeyDown win GLFW.Key'D
    space <- isKeyDown win GLFW.Key'Space
    shift <- isKeyDown win GLFW.Key'LeftShift
    let d1 = if w then      (0   :.0:.1:.()) else 0
        d2 = if d then d1 + ((-1):.0:.0:.()) else d1
        d3 = if s then d2 + (0:.0:.(-1):.()) else d2
        d4 = if a then d3 + (1:.0:.0:.()) else d3
        d5 = if space then d4 + (0:.1:.0:.()) else d4
        d6 = if shift then d5 + (0:.(-1):.0:.()) else d5
    return d6

getTime :: IO Float
getTime = (realToFrac . utctDayTime) `fmap` getCurrentTime

--startGL :: RayTrace Float a => a -> IO ()
startGL bvh = do
    let img = rayTraceImgCoh myCam bvh
    win <- openWindow
    initGL win
    GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled
    cpos <- newIORef 0
    ppos <- newIORef (0,0)
    fCount <- newIORef 0
    lastFrame <- newIORef 0
    to <- imgToTexObj img
    prog <- glProg to
    startTime <- getTime
    let galaxy = MonadicGalaxy prog
            (updateImage win cpos ppos startTime lastFrame fCount) to
    mainLoop win $ [galaxy] -|> []
  where
    updateImage :: GLFW.Window ->
        IORef (Vec3 Float) -> IORef (Double,Double) ->
        Float ->
        IORef Float ->
        IORef Int ->
        GL.TextureObject -> IO GL.TextureObject
    updateImage win playerPos playerRot startTime lastFrame fCount _ = do
        (w, h) <- GLFW.getWindowSize win
        (x, y) <- GLFW.getCursorPos win
        (rx, ry) <- readIORef playerRot

        cpos <- readIORef playerPos
        mpos <- camMovement win

        let (dx, dy) = (x - (fromIntegral $ w `div` 2),
                        y - (fromIntegral $ h `div` 2))
            (nx, ny) = (realToFrac $ rx + (dx * 0.004),
                        realToFrac $ ry + (dy * 0.004))
            newOrigin = cpos + Vec.map (*0.1) mpos
            newMat = Vec.rotationEuler ((-ny):.(-nx):.0:.())
            img = rayTraceImgCoh myCam{
                viewMatrix = newMat,
                viewOrigin = newOrigin} bvh

        writeIORef playerPos newOrigin
        GLFW.setCursorPos win (fromIntegral $ w `div` 2)
                              (fromIntegral $ h `div` 2)
        writeIORef playerRot (realToFrac nx, realToFrac ny)

        curTime <- getTime
        fc <- readIORef fCount
        lastF <- readIORef lastFrame
        let delta = curTime - lastF
            fps = fromIntegral fc / (curTime - startTime)
        putStrLn $ "Delta: " ++ show delta
        putStrLn $ "Avg FPS: " ++ show fps
        putStrLn $ "Current FPS: " ++ show (1/delta)

        writeIORef lastFrame curTime
        modifyIORef' fCount (+1)
        --exitSuccess
        imgToTexObj img

main :: IO ()
main = do
--    startGL scene
    startGL $ flatten myBVH