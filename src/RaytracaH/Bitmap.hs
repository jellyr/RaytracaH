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
