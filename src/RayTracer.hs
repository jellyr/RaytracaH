module RayTracer where

import Bitmap
import Camera
import Color
import Light
import Screen
import Options
import Primitive
import Ray
import Util

import qualified Data.Vec as Vec
import qualified Data.Vector as V

data PrimitiveIntersection a = PrimitiveIntersection (Maybe a) IntersectionResult

fileWithRenderedImage :: V.Vector AnyPrimitive -> V.Vector Light -> Camera -> RayTracerOptions -> PPMFile
fileWithRenderedImage primitives lights camera options = 
    PPMFile (PPMFileHeader screenW screenH 255) (render screen primitives lights camera options)
    where
        screenW = imgWidth options
        screenH = imgHeight options
        screen = Screen screenW screenH

render :: Screen -> V.Vector AnyPrimitive -> V.Vector Light -> Camera -> RayTracerOptions -> Pixels
render screen primitives lights camera options
    | V.null primitives = V.map (const $ toPixel (backgroundColor options)) primaryRays
    | otherwise = 
        V.map (traceRay options primitives lights) primaryRays
    where
        primaryRays = generatePrimaryRays screen camera

-- TODO: calculate color in more elegant way, take into account light color
traceRay :: RayTracerOptions -> V.Vector AnyPrimitive -> V.Vector Light -> Ray -> Pixel
traceRay options primitives lights ray = 
    case primitiveWithintersection of PrimitiveIntersection (Just hitPrimitive) (Intersection hitDistance) -> 
                                          sumLightsEffect options lights primitives hitPrimitive hitDistance ray
                                      _ -> 
                                          toPixel $ backgroundColor options
    where
        primitiveWithintersection = findNearestIntersectingPrimitive primitives ray (infinityDistance options)

sumLightsEffect :: RayTracerOptions -> V.Vector Light -> V.Vector AnyPrimitive -> AnyPrimitive -> Float -> Ray -> Pixel
sumLightsEffect options lights primitives hitPrimitive hitDistance hitRay = 
    toPixel $ fmap (\c -> ceiling (diffuse * fromIntegral c)) (color hitPrimitive)
    where
        diffuse = min 1.0 (V.sum $ V.map (\light ->
                if isHitPrimitiveInShadow options primitives light hitPrimitive hitRay hitDistance == True then
                    0.0
                else
                    calculateDiffuseForHitPrimitive light hitDistance hitPrimitive hitRay
            ) lights)

isHitPrimitiveInShadow :: RayTracerOptions -> V.Vector AnyPrimitive -> Light -> AnyPrimitive -> Ray -> Float -> Bool
isHitPrimitiveInShadow options primitives light hitPrimitive prevRay hitDistance = 
    let
        shadowRay = createShadowRay options light prevRay hitDistance hitPrimitive
        primitiveWithintersection = findNearestIntersectingPrimitive primitives shadowRay (infinityDistance options)
    in
        case primitiveWithintersection of PrimitiveIntersection _ (Intersection _) -> True
                                          _ -> False

createShadowRay :: RayTracerOptions -> Light -> Ray -> Float -> AnyPrimitive -> Ray
createShadowRay options light prevRay hitDistance hitPrimitive = 
    Ray (pHit + multvs nHit (shadowBias options)) (-vecL)
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
