module Ray where

import Test.QuickCheck (Gen(..), Arbitrary(..), choose)

import Data.Vec
import qualified Data.Vector as V

import Camera
import Math
import Screen

data Ray = Ray {
    origin :: Vector3D,
    direction :: Vector3D
} deriving (Show)

type PixelsCoords = V.Vector (Int, Int)

pointOnRay :: Ray -> Float -> Vector3D
pointOnRay ray distance = 
    origin ray + multvs (direction ray) distance

generatePrimaryRays :: Screen -> Camera -> V.Vector Ray
generatePrimaryRays screen camera = 
    V.map (createRay camera) pixelsCameraCoords
    where
        pixels = screenPixels screen
        pixelsCameraCoords = pixelsToCameraCoords screen (fov camera) pixels

createRay :: Camera -> Vector3D -> Ray
createRay camera@(Camera eyePosition _ _ _) pixelCoords = 
    Ray rayOrigin dir
    where
        rayOrigin = pointToCameraSpace camera eyePosition
        dir = normalize (pointToCameraSpace camera pixelCoords)

screenPixels :: Screen -> PixelsCoords
screenPixels (Screen screenW screenH) = 
    V.fromList [ (y, x) | y <- [1..screenH], x <- [1..screenW] ]

pixelsToCameraCoords :: Screen -> Float -> PixelsCoords -> V.Vector Vector3D
pixelsToCameraCoords screen fov = 
    V.map (\(y, x) -> screenCoordsToCameraCoords fov $ ndcToScreenCoords screen $ pixelToNdc x y)
    where
        pixelToNdc = pixelToNdcCoords screen

pixelToNdcCoords :: Screen -> Int -> Int-> Vector2D
pixelToNdcCoords (Screen screenW screenH) x y = 
    Vec2F ((fromIntegral x + 0.5)/fromIntegral screenW) ((fromIntegral y + 0.5)/fromIntegral screenH)

ndcToScreenCoords :: Screen  -> Vector2D -> Vector2D
ndcToScreenCoords (Screen screenW screenH) (Vec2F x y) = 
    Vec2F ((2*x - 1)*aspectRatio) (1 - 2*y)
    where
        aspectRatio = fromIntegral screenW / fromIntegral screenH

screenCoordsToCameraCoords :: Float -> Vector2D -> Vector3D
screenCoordsToCameraCoords fov (Vec2F x y) = 
    Vec3F (x * fovTanValue) (y * fovTanValue) (-1.0)
    where
        fovTanValue = tan (deg2rad (fov / 2))

instance Arbitrary Ray where
    arbitrary = do
        rOrigin <- arbitrary :: (Gen AnyVector3D)
        rDirection <- arbitrary :: (Gen AnyVector3D)
        return $ Ray (v3d rOrigin) (v3d rDirection)
