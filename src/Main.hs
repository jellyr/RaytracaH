{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Vec as Vec
import qualified Data.Vector as V
import Data.Time

import Bitmap
import Primitive
import Sphere
import Plane
import RayTracer

-- temporary stuff, only for result testing purpose
outputFileName :: String
outputFileName = "../test.ppm"

imgWidth :: Int
imgWidth = 640

imgHeight :: Int
imgHeight = 480

sampleSpheres :: V.Vector AnyPrimitive
sampleSpheres = V.fromList [AnyPrimitive $ Plane (Vec.Vec3F 0.0 (-10.0) (-30.0)) (Vec.Vec3F 0.0 1.0 0.0) (Color 255 0 0), 
                            AnyPrimitive $ Sphere (Vec.Vec3F (-5.0) 0.0 (-30.0)) 2.0 (Color 255 0 0),
                            AnyPrimitive $ Sphere (Vec.Vec3F 0.0 5.0 (-30.0)) 2.0 (Color 0 255 0),
                            AnyPrimitive $ Sphere (Vec.Vec3F 5.0 0.0 (-30.0)) 2.0 (Color 255 0 0),
                            AnyPrimitive $ Sphere (Vec.Vec3F 0.0 (-5.0) (-30.0)) 2.0 (Color 0 0 255),
                            AnyPrimitive $ Sphere (Vec.Vec3F 0.0 0.0 (-30.0)) 1.0 (Color 227 166 0)]

main :: IO ()
main = do
    putStrLn "Welcome to raytracaH\n"
    startTime <- getCurrentTime
    writeAsciiPPMFile outputFileName (fileWithRenderedImage imgWidth imgHeight sampleSpheres)
    endTime <- getCurrentTime
    putStr "Work finished, total time: "
    print $ diffUTCTime endTime startTime
    return ()
