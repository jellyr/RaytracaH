module Ray where

import Data.Vec
import qualified Data.Vector as V

import Camera
import Screen
import Util

data Ray = Ray {
    origin :: Vector3D,
    direction :: Vector3D
} deriving (Show)

type PixelsCoords = V.Vector (Int, Int)

generatePrimaryRays :: Screen -> Float -> Camera -> V.Vector Ray
generatePrimaryRays screen fov camera = 
    V.map (Ray (pointToCameraSpace camera eyePosition) . normalize . pointToCameraSpace camera) pixelsCameraCoords
    where
        (Camera eyePosition _ _) = camera
        pixels = screenPixels screen
        pixelsCameraCoords = pixelsToCameraCoords screen fov pixels

screenPixels :: Screen -> PixelsCoords
screenPixels (Screen screenW screenH) = 
    V.fromList [ (y, x) | y <- [1..screenH], x <- [1..screenW] ]

pixelsToCameraCoords :: Screen -> Float -> PixelsCoords -> V.Vector Vector3D
pixelsToCameraCoords screen fov = 
    V.map (\(y, x) -> screenCoordsToCameraCoords fov $ ndcToScreenCoords screen $ pixelToNdc x y)
    where
        pixelToNdc = pixelToNdcCoords screen

pixelToNdcCoords :: Screen -> Int -> Int-> Vector2D
pixelToNdcCoords (Screen screenW screenH) x y = Vec2F ((fromIntegral x + 0.5)/fromIntegral screenW) ((fromIntegral y + 0.5)/fromIntegral screenH)

ndcToScreenCoords :: Screen  -> Vector2D -> Vector2D
ndcToScreenCoords (Screen screenW screenH) (Vec2F x y) = Vec2F ((2*x - 1)*aspectRatio) (1 - 2*y)
    where
        aspectRatio = fromIntegral screenW / fromIntegral screenH

screenCoordsToCameraCoords :: Float -> Vector2D -> Vector3D
screenCoordsToCameraCoords fov (Vec2F x y) = Vec3F (x * fovTanValue) (y * fovTanValue) (-1.0)
    where
        fovTanValue = tan (deg2rad (fov / 2))
