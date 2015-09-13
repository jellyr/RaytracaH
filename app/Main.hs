module Main where

import qualified Data.Vec as Vec
import qualified Data.Vector as V
import Data.Time

import Bitmap
import Camera
import Color
import Light
import Material
import Primitive
import Scene
import Sphere
import Options
import Plane
import RayTracer

-- TODO: temporary stuff, only for result testing purpose
-- in future all config and input will be loaded from file
outputFileName :: String
outputFileName = "test.ppm"

outputImgWidth :: Int
outputImgWidth = 640

outputImgHeight :: Int
outputImgHeight = 480

sampleLights :: V.Vector Light
sampleLights = V.fromList [Directional (Vec.normalize $ (Vec.Vec3F 0.0 0.0 0.0) - (Vec.Vec3F 0.0 1.0 1.0)) 0.8 (Color 255 255 255),
                           Directional (Vec.normalize $ (Vec.Vec3F 0.0 0.0 0.0) - (Vec.Vec3F (-1.0) 1.0 1.0)) 0.3 (Color 255 255 255),
                           Directional (Vec.normalize $ (Vec.Vec3F 0.0 0.0 0.0) - (Vec.Vec3F 1.0 1.0 1.0)) 0.1 (Color 255 255 255)]

diffusiveRedMaterial :: Material
diffusiveRedMaterial = DiffusiveMaterial $ Color 255 0 0

diffusiveGreenMaterial :: Material
diffusiveGreenMaterial = DiffusiveMaterial $ Color 0 255 0

diffusiveBlueMaterial :: Material
diffusiveBlueMaterial = DiffusiveMaterial $ Color 0 0 255

sampleSpheres :: V.Vector AnyPrimitive
sampleSpheres = V.fromList [AnyPrimitive $ Plane (Vec.Vec3F 0.0 (-4.0) 0.0) (Vec.Vec3F 0.0 1.0 0.0) diffusiveRedMaterial, 
                            AnyPrimitive $ Sphere (Vec.Vec3F (-5.0) 0.0 (-5.0)) 1.0 diffusiveRedMaterial,
                            AnyPrimitive $ Sphere (Vec.Vec3F 0.0 0.0 (-5.0)) 2.0 diffusiveGreenMaterial,
                            AnyPrimitive $ Sphere (Vec.Vec3F 4.0 0.0 (-3.0)) 1.0 diffusiveBlueMaterial,
                            AnyPrimitive $ Sphere (Vec.Vec3F 0.0 5.0 (-5.0)) 2.0 (DiffusiveMaterial (Color 255 255 255)),
                            AnyPrimitive $ Sphere (Vec.Vec3F 0.0 3.0 (-7.0)) 2.0 (DiffusiveMaterial (Color 0 255 255)),
                            AnyPrimitive $ Sphere (Vec.Vec3F 0.0 0.0 0.0) 1.0 (DiffusiveMaterial (Color 227 166 0))]

sampleScene :: Scene
sampleScene = Scene sampleLights sampleSpheres

sampleCamera :: Camera
sampleCamera = Camera (Vec.Vec3F 0.0 2.0 20.0) (Vec.Vec3F 0 0 0) (Vec.Vec3F 0 1 0) 30.0

options :: RayTracerOptions
options = RayTracerOptions outputImgWidth outputImgHeight (Color 194 204 255) 10000.0 0.0001

main :: IO ()
main = do
    putStrLn "raytracaH\n"
    startTime <- getCurrentTime
    writeAsciiPPMFile outputFileName (fileWithRenderedImage sampleCamera options sampleScene)
    endTime <- getCurrentTime
    putStr ("Work finished, results saved to " ++ outputFileName ++ ", total time: ")
    print $ diffUTCTime endTime startTime
    return ()
