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

infinityDistance :: Float
infinityDistance = 10000.0

--TODO: rewrite it in more elegant way
render :: Primitive a => Int -> Int -> V.Vector a -> Pixels
render screenW screenH primitives
    | V.null primitives = V.map (const backgroundColor) primaryRays
    | otherwise = 
        V.map (\ray -> 
            let
                primitiveWithintersection = findNearestIntersectingPrimitive primitives ray infinityDistance (Nothing, NoIntersection)
            in
                case primitiveWithintersection of (Just primitive, Intersection t) -> calculateColor t primitive ray
                                                  _ -> backgroundColor
                                  
            ) primaryRays
    where
        screen = Screen screenW screenH
        primaryRays = generatePrimaryRays screen 30.0 (Vec.Vec3F 0 0 0)

findNearestIntersectingPrimitive :: Primitive a => V.Vector a -> Ray -> Float -> (Maybe a, IntersectionResult) -> (Maybe a, IntersectionResult)
findNearestIntersectingPrimitive primitives ray tNearest result
    | V.null primitives = result
    | otherwise = 
        case intersection of NoIntersection -> proceedWithNoIntersection
                             Intersection t -> 
                                if t < tNearest then 
                                    findInTail t (Just currentPrimitive, intersection)
                                else 
                                    proceedWithNoIntersection
        where
            currentPrimitive = V.head primitives
            intersection = intersect currentPrimitive ray
            findInTail = findNearestIntersectingPrimitive (V.tail primitives) ray
            proceedWithNoIntersection = findInTail tNearest result

calculateColor :: Primitive a => Float -> a -> Ray -> Pixel
calculateColor t primitive (Ray rOrigin rDirectory) = 
    let
        pHit = rOrigin + rDirectory * Vec.Vec3F t t t
        nHit = normalAtHitPoint primitive pHit
        intensity = max 0.0 (Vec.dot nHit (rDirectory * (-1.0)))
        primitiveColor = fmap (\c -> ceiling (intensity * fromIntegral c)) (color primitive)
    in
        toPixel primitiveColor
