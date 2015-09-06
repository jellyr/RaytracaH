module RayTracer where

import Bitmap
import Screen
import Primitive
import Ray

import qualified Data.Vec as Vec
import qualified Data.Vector as V

fileWithRenderedImage :: Primitive a => Int -> Int -> V.Vector a -> PPMFile
fileWithRenderedImage screenW screenH primitives = 
    PPMFile (PPMFileHeader screenW screenH 255) (render screenW screenH primitives)

backgroundColor :: Pixel
backgroundColor = Pixel 194 204 255

--TODO: rewrite it in more elegant way
render :: Primitive a => Int -> Int -> V.Vector a -> Pixels
render screenW screenH primitives
    | V.null primitives = V.map (const backgroundColor) primaryRays
    | otherwise = 
        V.map (\ray -> 
            let
                primitiveF = V.filter (\p -> intersect p ray /= NoIntersection) primitives
            in
                if V.null primitiveF then
                    backgroundColor
                else
                    let
                        primitive = V.head primitiveF
                        intersection = intersect primitive ray
                    in
                        case intersection of NoIntersection -> backgroundColor
                                             Intersection t -> calculateColor t primitive ray
            ) primaryRays
    where
        screen = Screen screenW screenH
        primaryRays = generatePrimaryRays screen 30.0 (Vec.Vec3F 0 0 0)



calculateColor :: Primitive a => Float -> a -> Ray -> Pixel
calculateColor t primitive (Ray rOrigin rDirectory) = 
    let
        pHit = rOrigin + rDirectory * Vec.Vec3F t t t
        nHit = normalAtHitPoint primitive pHit
        intensity = max 0.0 (Vec.dot nHit (rDirectory * (-1.0)))
        primitiveColor = fmap (\c -> ceiling (intensity * fromIntegral c)) (color primitive)
    in
        toPixel primitiveColor
