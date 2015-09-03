{-# LANGUAGE OverloadedStrings #-}

module Bitmap(
        PPMFileHeader(..),
        Pixel(..),
        Pixels,
        PPMFile(..),
        prepareFileContentsToSave
    ) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Vector as V

data PPMFileHeader = PPMFileHeader {
    typeHeader :: ByteString,
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

prepareFileContentsToSave :: PPMFile -> ByteString
prepareFileContentsToSave (PPMFile fileHeader filePixels) = BS.append (prepareHeaderForPPM fileHeader) (preparePixelsToSave filePixels)

prepareHeaderForPPM :: PPMFileHeader -> ByteString
prepareHeaderForPPM fileHeader = 
    foldl BS.append BS.empty [fileTypeHeader, "\n", dimensionsLine, "\n", maxColorValueLine, "\n"]
    where
        fileTypeHeader = typeHeader fileHeader
        dimensionsLine = prepareDimensionsLine fileHeader
        maxColorValueLine = BS.pack $ show $ maxColorValue fileHeader

prepareDimensionsLine :: PPMFileHeader -> ByteString 
prepareDimensionsLine fileHeader =
    foldl BS.append BS.empty [imgWidth, " ", imgHeight]
    where
        imgWidth = BS.pack $ show $ imageWidth fileHeader
        imgHeight = BS.pack $ show $ imageHeight fileHeader

preparePixelsToSave :: Pixels -> ByteString
preparePixelsToSave pp = V.foldl BS.append BS.empty pixelsAsText
    where 
        pixelsAsText = V.map (\p -> BS.append (BS.pack $ show p) " ") pp
