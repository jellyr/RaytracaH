{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bitmap
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Vector as V

outputFileName :: String
outputFileName = "test.ppm"

sampleHeader :: PPMFileHeader
sampleHeader = PPMFileHeader "P3" 2 2 255

samplePixels :: Pixels
samplePixels = V.fromList [Pixel 255 0 0, Pixel 0 255 0, Pixel 0 0 255, Pixel 0 0 0]

sampleFile :: PPMFile
sampleFile = PPMFile sampleHeader samplePixels

main :: IO ()
main = do
    putStrLn "Welcome to raytracaH\n"
    BS.writeFile outputFileName (prepareFileContentsToSave sampleFile)
    putStrLn "Work finished"
    return ()