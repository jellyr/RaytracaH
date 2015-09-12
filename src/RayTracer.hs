module RayTracer where

import Bitmap
import Camera
import Color
import Light
import Screen
import Primitive
import Ray
import Util

import qualified Data.Vec as Vec
import qualified Data.Vector as V

data PrimitiveIntersection a = PrimitiveIntersection (Maybe a) IntersectionResult

fileWithRenderedImage :: Int -> Int -> V.Vector AnyPrimitive -> V.Vector Light -> Camera -> PPMFile
fileWithRenderedImage screenW screenH primitives lights camera = 
    PPMFile (PPMFileHeader screenW screenH 255) (render screen primitives lights camera)
    where
        screen = Screen screenW screenH

backgroundColor :: Color Int
backgroundColor = Color 194 204 255

infinityDistance :: Float
infinityDistance = 10000.0

render :: Screen -> V.Vector AnyPrimitive -> V.Vector Light -> Camera -> Pixels
render screen primitives lights camera
    | V.null primitives = V.map (const $ toPixel backgroundColor) primaryRays
    | otherwise = 
        V.map (traceRay primitives lights) primaryRays
    where
        primaryRays = generatePrimaryRays screen camera

-- TODO: calculate color in more elegant way, take into account light color
traceRay :: V.Vector AnyPrimitive -> V.Vector Light -> Ray -> Pixel
traceRay primitives lights ray = 
    case primitiveWithintersection of PrimitiveIntersection (Just hitPrimitive) (Intersection hitDistance) -> 
                                          sumLightsEffect lights primitives hitPrimitive hitDistance ray
                                      _ -> 
                                          toPixel backgroundColor
    where
        primitiveWithintersection = findNearestIntersectingPrimitive primitives ray infinityDistance

sumLightsEffect :: V.Vector Light -> V.Vector AnyPrimitive -> AnyPrimitive -> Float -> Ray -> Pixel
sumLightsEffect lights primitives hitPrimitive hitDistance hitRay = 
    toPixel $ fmap (\c -> ceiling (diffuse * fromIntegral c)) (color hitPrimitive)
    where
        diffuse = min 1.0 (V.sum $ V.map (\light ->
                if isHitPrimitiveInShadow primitives light hitPrimitive hitRay hitDistance == True then
                    0.0
                else
                    calculateDiffuseForHitPrimitive light hitDistance hitPrimitive hitRay
            ) lights)

isHitPrimitiveInShadow :: V.Vector AnyPrimitive -> Light -> AnyPrimitive -> Ray -> Float -> Bool
isHitPrimitiveInShadow primitives light hitPrimitive prevRay hitDistance = 
    let
        shadowRay = createShadowRay light prevRay hitDistance hitPrimitive
        primitiveWithintersection = findNearestIntersectingPrimitive primitives shadowRay infinityDistance
    in
        case primitiveWithintersection of PrimitiveIntersection _ (Intersection _) -> True
                                          _ -> False

-- TODO: move bias to options
createShadowRay :: Light -> Ray -> Float -> AnyPrimitive -> Ray
createShadowRay light prevRay hitDistance hitPrimitive = 
    Ray (pHit + multvs nHit 0.001) (-vecL)
    where
        pHit = rayHitPoint prevRay hitDistance
        nHit = normalAtHitPoint hitPrimitive pHit
        vecL = lightDir light

findNearestIntersectingPrimitive :: V.Vector AnyPrimitive -> Ray -> Float -> PrimitiveIntersection AnyPrimitive
findNearestIntersectingPrimitive primitives ray distanceNearest = 
    findNearestIntersectingPrimitiveIter primitives ray distanceNearest (PrimitiveIntersection Nothing NoIntersection)

findNearestIntersectingPrimitiveIter :: V.Vector AnyPrimitive -> Ray -> Float -> PrimitiveIntersection AnyPrimitive -> PrimitiveIntersection AnyPrimitive
findNearestIntersectingPrimitiveIter primitives ray distanceNearest result
    | V.null primitives = result
    | otherwise = 
        case intersection of NoIntersection -> proceedWithNoIntersection
                             Intersection distance -> 
                                 if distance < distanceNearest then 
                                     findInTail distance (PrimitiveIntersection (Just currentPrimitive) intersection)
                                 else 
                                     proceedWithNoIntersection
        where
            currentPrimitive = V.head primitives
            intersection = currentPrimitive `intersect` ray
            findInTail = findNearestIntersectingPrimitiveIter (V.tail primitives) ray
            proceedWithNoIntersection = findInTail distanceNearest result

calculateDiffuseForHitPrimitive :: Light -> Float -> AnyPrimitive -> Ray -> Float
calculateDiffuseForHitPrimitive light hitDistance hitPrimitive ray =
    let
        pHit = rayHitPoint ray hitDistance
        nHit = normalAtHitPoint hitPrimitive pHit
        diffuse = lightIntensity light * max 0.0 (Vec.dot nHit (-1.0 * lightDir light))
    in
        diffuse

rayHitPoint :: Ray -> Float -> Vector3D
rayHitPoint (Ray rOrigin rDirectory) distance = rOrigin + multvs rDirectory distance
