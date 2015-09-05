module Ray where

import qualified Data.Vec as Vec
import qualified Data.Vector as V

import RayTracer
import Util

data Ray = Ray {
    origin :: Vec.Packed (Vec.Vec3 Float),
    direction :: Vec.Packed (Vec.Vec3 Float)
} deriving (Show)

type PixelsCoords = V.Vector (Int, Int)

type Vector2D = Vec.Packed (Vec.Vec2 Float)
type Vector3D = Vec.Packed (Vec.Vec3 Float)

-- TODO: eye must be (0,0,0) now, implement transformation of origin point
generatePrimaryRays :: Screen -> Float -> Vector3D -> V.Vector Ray
generatePrimaryRays screen fov eye = 
    V.map (\coords -> Ray eye (Vec.normalize (coords - eye))) pixelsCameraCoords
    where
        pixels = screenPixels screen
        screenCoords = ndcToScreenCoords screen $ pixelsToNdc screen pixels
        pixelsCameraCoords = screenCoordsToCameraCoords fov screenCoords

screenPixels :: Screen -> PixelsCoords
screenPixels (Screen screenW screenH) = 
    V.fromList [ (y, x) | y <- [1..screenH], x <- [1..screenW] ]

pixelsToNdc :: Screen -> PixelsCoords -> V.Vector Vector2D
pixelsToNdc (Screen screenW screenH) = 
    V.map (\(y, x) -> Vec.Vec2F ((fromIntegral x + 0.5)/fromIntegral screenW) ((fromIntegral y + 0.5)/fromIntegral screenH))

ndcToScreenCoords :: Screen -> V.Vector Vector2D -> V.Vector Vector2D
ndcToScreenCoords (Screen screenW screenH) = 
    V.map (\(Vec.Vec2F x y) -> Vec.Vec2F ((2*x - 1)*aspectRatio) (1 - 2*y))
    where
        aspectRatio = fromIntegral screenW/fromIntegral screenH

screenCoordsToCameraCoords :: Float -> V.Vector Vector2D -> V.Vector Vector3D
screenCoordsToCameraCoords fov = 
    V.map (\(Vec.Vec2F x y) -> Vec.Vec3F (x * fovTanValue) (y * fovTanValue) (-1.0))
    where 
        fovTanValue = tan (deg2rad (fov / 2))
