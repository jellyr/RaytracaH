{-

Copyright 2015 Rafał Nowak

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

-}

{-# LANGUAGE OverloadedStrings #-}

module RaytracaH.Bitmap(
        PPMFileHeader(..),
        Pixel(..),
        Pixels,
        PPMFile(..),
        writeAsciiPPMFile
    ) where

import Data.ByteString.Lazy (ByteString)
import System.IO
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Vector as V

data PPMFileHeader = PPMFileHeader {
    imageWidth :: Int,
    imageHeight :: Int,
    maxColorValue :: Int
}

data Pixel = Pixel {
    r :: Int,
    g :: Int,
    b :: Int
}

type Pixels = V.Vector Pixel

data PPMFile = PPMFile PPMFileHeader Pixels

instance Show Pixel where
    show pixel = show rVal ++ " " ++ show gVal ++ " " ++ show bVal
        where
            rVal = r pixel
            gVal = g pixel
            bVal = b pixel

writeAsciiPPMFile :: String -> PPMFile -> IO ()
writeAsciiPPMFile fileName (PPMFile fileHeader pixels) = do
    file <- openFile fileName WriteMode
    BS.hPutStr file (prepareHeaderForAsciiPPM fileHeader)
    writePixelsToAsciiPPM file pixels
    hClose file
    return ()

prepareHeaderForAsciiPPM :: PPMFileHeader -> ByteString
prepareHeaderForAsciiPPM fileHeader = 
    foldl BS.append BS.empty $ linesWithNewLineBetween [fileTypeHeader, dimensionsLine, maxColorValueLine]
    where
        fileTypeHeader = asciiPPMFileMagicNumber
        dimensionsLine = prepareDimensionsLine fileHeader
        maxColorValueLine = BS.pack $ show $ maxColorValue fileHeader

linesWithNewLineBetween :: [ByteString] -> [ByteString]
linesWithNewLineBetween = concatMap (\line -> [line, "\n"])

asciiPPMFileMagicNumber :: ByteString
asciiPPMFileMagicNumber = "P3"

prepareDimensionsLine :: PPMFileHeader -> ByteString 
prepareDimensionsLine fileHeader =
    foldl BS.append BS.empty [imgWidth, " ", imgHeight]
    where
        imgWidth = BS.pack $ show $ imageWidth fileHeader
        imgHeight = BS.pack $ show $ imageHeight fileHeader

writePixelsToAsciiPPM :: Handle -> Pixels -> IO ()
writePixelsToAsciiPPM file pixels = do
    V.mapM_ (BS.hPutStr file . addTrailingSpace) pixels
    return ()
    where
        asByteString pixel = BS.pack $ show pixel
        addTrailingSpace pixel = BS.append (asByteString pixel) " "
