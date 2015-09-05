{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bitmap
import qualified Data.Vector as V
import qualified Data.Vec as Vec
import qualified RayTracer as RT
import qualified Ray as R
import Primitive
import Sphere

-- temporary stuff, only for result testing purpose
outputFileName :: String
outputFileName = "../test.ppm"

imgWidth :: Int
imgWidth = 640
imgHeight :: Int
imgHeight = 480

sampleHeader :: PPMFileHeader
sampleHeader = PPMFileHeader imgWidth imgHeight 255

screen :: RT.Screen
screen = RT.Screen imgWidth imgHeight

sampleSphere :: Sphere
sampleSphere = Sphere (Vec.Vec3F 0.0 0.0 (-10.0)) 2.0

samplePixels :: Pixels
samplePixels = 
    V.map (\ray -> 
        let
            intersection = intersect sampleSphere ray
        in
            case intersection of NoIntersection -> Pixel 194 204 255
                                 Intersection t -> calculateColor t sampleSphere ray
        ) primaryRays
    where
        primaryRays = R.generatePrimaryRays screen 30.0 (Vec.Vec3F 0 0 0)

calculateColor :: Float -> Sphere -> R.Ray -> Pixel
calculateColor t (Sphere sphereCenter _) (R.Ray rOrigin rDirectory) = 
    let
        pHit = rOrigin + rDirectory * Vec.Vec3F t t t
        nHit = Vec.normalize (pHit - sphereCenter)
        intensity = max 0.0 (Vec.dot nHit (rDirectory * (-1.0)))
    in
        Pixel (ceiling (intensity * 255)) 0 0

sampleFile :: PPMFile
sampleFile = PPMFile sampleHeader samplePixels

main :: IO ()
main = do
    putStrLn "Welcome to raytracaH\n"
    writeAsciiPPMFile outputFileName sampleFile
    putStrLn "Work finished"
    return ()
