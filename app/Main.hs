module Main where

import qualified Data.Vec as Vec
import qualified Data.Vector as V
import Data.Time

import Bitmap
import Camera
import Color
import Light
import Primitive
import Sphere
import Plane
import RayTracer

-- TODO: temporary stuff, only for result testing purpose
-- in future all config and input will be loaded from file
outputFileName :: String
outputFileName = "test.ppm"

imgWidth :: Int
imgWidth = 640

imgHeight :: Int
imgHeight = 480

sampleLights :: V.Vector Light
sampleLights = V.fromList [Directional (Vec.Vec3F 0.0 (-1.0) 0.0) (Color 255 255 255)]

sampleSpheres :: V.Vector AnyPrimitive
sampleSpheres = V.fromList [AnyPrimitive $ Plane (Vec.Vec3F 0.0 (-5.0) 0.0) (Vec.Vec3F 0.0 1.0 0.0) (Color 255 0 0), 
                            AnyPrimitive $ Sphere (Vec.Vec3F (-5.0) 0.0 (-5.0)) 1.0 (Color 255 0 0),
                            AnyPrimitive $ Sphere (Vec.Vec3F 0.0 0.0 (-5.0)) 2.0 (Color 0 255 0),
                            AnyPrimitive $ Sphere (Vec.Vec3F 4.0 0.0 (-3.0)) 1.0 (Color 0 0 255),
                            AnyPrimitive $ Sphere (Vec.Vec3F 0.0 5.0 (-5.0)) 2.0 (Color 255 255 255),
                            AnyPrimitive $ Sphere (Vec.Vec3F 0.0 3.0 (-7.0)) 2.0 (Color 0 255 255),
                            AnyPrimitive $ Sphere (Vec.Vec3F 0.0 0.0 0.0) 1.0 (Color 227 166 0)]

sampleCamera :: Camera
sampleCamera = Camera (Vec.Vec3F 0.0 5.0 20.0) (Vec.Vec3F 0 0 0) (Vec.Vec3F 0 1 0)

main :: IO ()
main = do
    putStrLn "raytracaH\n"
    startTime <- getCurrentTime
    writeAsciiPPMFile outputFileName (fileWithRenderedImage imgWidth imgHeight sampleSpheres sampleLights sampleCamera)
    endTime <- getCurrentTime
    putStr ("Work finished, results saved to " ++ outputFileName ++ ", total time: ")
    print $ diffUTCTime endTime startTime
    return ()
