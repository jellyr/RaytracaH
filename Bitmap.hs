{-# LANGUAGE OverloadedStrings #-}

module Bitmap(
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

asciiPPMFileMagicNumber :: ByteString
asciiPPMFileMagicNumber = "P3"

writeAsciiPPMFile :: String -> PPMFile -> IO ()
writeAsciiPPMFile fileName (PPMFile fileHeader pixels) = do
    file <- openFile fileName WriteMode
    BS.hPutStr file (prepareHeaderForAsciiPPM fileHeader)
    writePixelsToAsciiPPM file pixels
    hClose file
    return ()

prepareHeaderForAsciiPPM :: PPMFileHeader -> ByteString
prepareHeaderForAsciiPPM fileHeader = 
    foldl BS.append BS.empty [fileTypeHeader, "\n", dimensionsLine, "\n", maxColorValueLine, "\n"]
    where
        fileTypeHeader = asciiPPMFileMagicNumber
        dimensionsLine = prepareDimensionsLine fileHeader
        maxColorValueLine = BS.pack $ show $ maxColorValue fileHeader

prepareDimensionsLine :: PPMFileHeader -> ByteString 
prepareDimensionsLine fileHeader =
    foldl BS.append BS.empty [imgWidth, " ", imgHeight]
    where
        imgWidth = BS.pack $ show $ imageWidth fileHeader
        imgHeight = BS.pack $ show $ imageHeight fileHeader

writePixelsToAsciiPPM :: Handle -> Pixels -> IO ()
writePixelsToAsciiPPM file pixels = do
    V.mapM_ (\pixel -> BS.hPutStr file (pixelWithTrailingSpace pixel)) pixels
    return ()
    where
        pixelAsString pixel = BS.pack $ show pixel
        pixelWithTrailingSpace pixel = BS.append (pixelAsString pixel) " "
