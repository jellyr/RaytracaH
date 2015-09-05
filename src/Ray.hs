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
        pixelsCameraCoords = pixelsToCameraCoords screen fov pixels

screenPixels :: Screen -> PixelsCoords
screenPixels (Screen screenW screenH) = 
    V.fromList [ (y, x) | y <- [1..screenH], x <- [1..screenW] ]

pixelsToCameraCoords :: Screen -> Float -> PixelsCoords -> V.Vector Vector3D
pixelsToCameraCoords (Screen screenW screenH) fov = 
    V.map (\(y, x) -> screenCoordsToCamera $ ndcToScreenCoords $ ndcCoords x y)
    where
        ndcCoords x y = Vec.Vec2F ((fromIntegral x + 0.5)/fromIntegral screenW) ((fromIntegral y + 0.5)/fromIntegral screenH)
        ndcToScreenCoords (Vec.Vec2F x y) = Vec.Vec2F ((2*x - 1)*aspectRatio) (1 - 2*y)
        screenCoordsToCamera (Vec.Vec2F x y) = Vec.Vec3F (x * fovTanValue) (y * fovTanValue) (-1.0)
        aspectRatio = fromIntegral screenW/fromIntegral screenH
        fovTanValue = tan (deg2rad (fov / 2))
