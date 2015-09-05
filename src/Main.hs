{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Vec as Vec
import Bitmap
import Sphere
import qualified RayTracer as RT
 
-- temporary stuff, only for result testing purpose
outputFileName :: String
outputFileName = "../test.ppm"

imgWidth :: Int
imgWidth = 640

imgHeight :: Int
imgHeight = 480

sampleSphere :: Sphere
sampleSphere = Sphere (Vec.Vec3F 0.0 0.0 (-10.0)) 2.0

main :: IO ()
main = do
    putStrLn "Welcome to raytracaH\n"
    writeAsciiPPMFile outputFileName (RT.fileWithRender imgWidth imgHeight sampleSphere)
    putStrLn "Work finished"
    return ()
