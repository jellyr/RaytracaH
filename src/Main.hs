{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bitmap
import qualified Data.Vector as V
import qualified Data.Vec as Vec
import qualified RayTracer as RT
import qualified Ray as R
import qualified Primitive as P

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

sampleSphere :: P.Sphere
sampleSphere = P.Sphere (Vec.Vec3F 0.0 0.0 (-10.0)) 2.0

samplePixels :: Pixels
samplePixels = 
    V.map (\ray -> 
        let
            intersection = P.intersect sampleSphere ray
        in
            if intersection == P.NoIntersection then
                Pixel 194 204 255
            else
                Pixel 255 0 0
        ) primaryRays
    where
        primaryRays = R.generatePrimaryRays screen 30.0 (Vec.Vec3F 0 0 0)

sampleFile :: PPMFile
sampleFile = PPMFile sampleHeader samplePixels

main :: IO ()
main = do
    putStrLn "Welcome to raytracaH\n"
    writeAsciiPPMFile outputFileName sampleFile
    putStrLn "Work finished"
    return ()
