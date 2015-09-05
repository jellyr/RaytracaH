module RayTracer where

import Bitmap
import Screen
import Primitive
import Sphere
import Ray

import qualified Data.Vec as Vec
import qualified Data.Vector as V

fileWithRender :: Int -> Int -> Sphere -> PPMFile
fileWithRender screenW screenH sphere = 
    PPMFile (PPMFileHeader screenW screenH 255) (render screenW screenH sphere)

render :: Int -> Int -> Sphere -> Pixels
render screenW screenH sphere = 
    V.map (\ray -> 
        let
            intersection = intersect sphere ray
        in
            case intersection of NoIntersection -> Pixel 194 204 255
                                 Intersection t -> calculateColor t sphere ray
        ) primaryRays
    where
        screen = Screen screenW screenH
        primaryRays = generatePrimaryRays screen 30.0 (Vec.Vec3F 0 0 0)

calculateColor :: Float -> Sphere -> Ray -> Pixel
calculateColor t (Sphere sphereCenter _) (Ray rOrigin rDirectory) = 
    let
        pHit = rOrigin + rDirectory * Vec.Vec3F t t t
        nHit = Vec.normalize (pHit - sphereCenter)
        intensity = max 0.0 (Vec.dot nHit (rDirectory * (-1.0)))
    in
        Pixel (ceiling (intensity * 255)) 0 0
