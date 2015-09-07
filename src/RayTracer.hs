module RayTracer where

import Bitmap
import Color
import Screen
import Primitive
import Ray

import qualified Data.Vec as Vec
import qualified Data.Vector as V

fileWithRenderedImage :: Int -> Int -> V.Vector AnyPrimitive -> PPMFile
fileWithRenderedImage screenW screenH primitives = 
    PPMFile (PPMFileHeader screenW screenH 255) (render screen primitives)
    where
        screen = Screen screenW screenH

backgroundColor :: Pixel
backgroundColor = Pixel 194 204 255

infinityDistance :: Float
infinityDistance = 10000.0

--TODO: rewrite it in more elegant way
render :: Screen -> V.Vector AnyPrimitive -> Pixels
render screen primitives
    | V.null primitives = V.map (const backgroundColor) primaryRays
    | otherwise = 
        V.map (\ray -> 
            let
                primitiveWithintersection = findNearestIntersectingPrimitive primitives ray infinityDistance (Nothing, NoIntersection)
            in
                case primitiveWithintersection of (Just primitive, Intersection distance) -> calculateColor distance primitive ray
                                                  _ -> backgroundColor
                                  
            ) primaryRays
    where
        primaryRays = generatePrimaryRays screen 30.0 (Vec.Vec3F 0 0 0)

findNearestIntersectingPrimitive :: V.Vector AnyPrimitive -> Ray -> Float -> (Maybe AnyPrimitive, IntersectionResult) -> (Maybe AnyPrimitive, IntersectionResult)
findNearestIntersectingPrimitive primitives ray tNearest result
    | V.null primitives = result
    | otherwise = 
        case intersection of NoIntersection -> proceedWithNoIntersection
                             Intersection distance -> 
                                if distance < tNearest then 
                                    findInTail distance (Just currentPrimitive, intersection)
                                else 
                                    proceedWithNoIntersection
        where
            currentPrimitive = V.head primitives
            intersection = currentPrimitive `intersect` ray
            findInTail = findNearestIntersectingPrimitive (V.tail primitives) ray
            proceedWithNoIntersection = findInTail tNearest result

calculateColor :: Float -> AnyPrimitive -> Ray -> Pixel
calculateColor distance primitive (Ray rOrigin rDirectory) = 
    let
        pHit = rOrigin + rDirectory * Vec.Vec3F distance distance distance
        nHit = normalAtHitPoint primitive pHit
        intensity = max 0.0 (Vec.dot nHit (rDirectory * (-1.0)))
        primitiveColor = fmap (\c -> ceiling (intensity * fromIntegral c)) (color primitive)
    in
        toPixel primitiveColor
