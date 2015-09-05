{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bitmap
import qualified Data.Vector as V
import qualified Data.Vec as Vec
import qualified RayTracer as RT
import qualified Ray as R

outputFileName :: String
outputFileName = "../test.ppm"

imgWidth :: Int
imgWidth = 1280
imgHeight :: Int
imgHeight = 720

sampleHeader :: PPMFileHeader
sampleHeader = PPMFileHeader imgWidth imgHeight 255

screen :: RT.Screen
screen = RT.Screen imgWidth imgHeight

samplePixels :: Pixels
samplePixels = 
    V.map rayToPixel primaryRays
    where
        primaryRays = R.generatePrimaryRays screen 30.0 (Vec.Vec3F 0 0 0)
        rayToPixel (R.Ray _ direction) = 
            Pixel (ceiling (x * 255)) (ceiling (y * 255)) (ceiling (z * 255))
            where
                (Vec.Vec3F x y z) = (direction + Vec.Vec3F 1 1 1) * 0.5

sampleFile :: PPMFile
sampleFile = PPMFile sampleHeader samplePixels

main :: IO ()
main = do
    putStrLn "Welcome to raytracaH\n"
    writeAsciiPPMFile outputFileName sampleFile
    putStrLn "Work finished"
    return ()