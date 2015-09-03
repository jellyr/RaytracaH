{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bitmap
import qualified Data.Vector as V

outputFileName :: String
outputFileName = "test.ppm"

imgWidth :: Int
imgWidth = 1000
imgHeight :: Int
imgHeight = 1000

sampleHeader :: PPMFileHeader
sampleHeader = PPMFileHeader imgWidth imgHeight 255

samplePixels :: Pixels
samplePixels = V.fromList $ map (\_ -> Pixel 127 194 255) [1..imgWidth*imgHeight]

sampleFile :: PPMFile
sampleFile = PPMFile sampleHeader samplePixels

main :: IO ()
main = do
    putStrLn "Welcome to raytracaH\n"
    writeAsciiPPMFile outputFileName sampleFile
    putStrLn "Work finished"
    return ()