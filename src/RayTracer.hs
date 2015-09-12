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

shadowColor :: Color Int
shadowColor = Color 0 0 0

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
    toPixel $ V.foldl (\acc c -> sumColors acc c) (Color 0 0 0) colors
    where
        colors = 
            V.map (\light ->
                if isHitPrimitiveInShadow primitives light hitPrimitive hitRay hitDistance == True then
                  shadowColor
                else
                  calculateColorForHitPrimitive light hitDistance hitPrimitive hitRay
            ) lights

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
findNearestIntersectingPrimitive primitives ray tNearest = 
    findNearestIntersectingPrimitiveIter primitives ray tNearest (PrimitiveIntersection Nothing NoIntersection)

findNearestIntersectingPrimitiveIter :: V.Vector AnyPrimitive -> Ray -> Float -> PrimitiveIntersection AnyPrimitive -> PrimitiveIntersection AnyPrimitive
findNearestIntersectingPrimitiveIter primitives ray tNearest result
    | V.null primitives = result
    | otherwise = 
        case intersection of NoIntersection -> proceedWithNoIntersection
                             Intersection distance -> 
                                 if distance < tNearest then 
                                     findInTail distance (PrimitiveIntersection (Just currentPrimitive) intersection)
                                 else 
                                     proceedWithNoIntersection
        where
            currentPrimitive = V.head primitives
            intersection = currentPrimitive `intersect` ray
            findInTail = findNearestIntersectingPrimitiveIter (V.tail primitives) ray
            proceedWithNoIntersection = findInTail tNearest result

calculateColorForHitPrimitive :: Light -> Float -> AnyPrimitive -> Ray -> Color Int
calculateColorForHitPrimitive light hitDistance hitPrimitive ray@(Ray rOrigin rDirectory) = 
    let
        pHit = rayHitPoint ray hitDistance
        nHit = normalAtHitPoint hitPrimitive pHit
        intensity = lightIntensity light * max 0.0 (Vec.dot nHit (rDirectory * (-1.0)))
        primitiveColor = fmap (\c -> ceiling (intensity * fromIntegral c)) (color hitPrimitive)
    in
        primitiveColor

rayHitPoint :: Ray -> Float -> Vector3D
rayHitPoint (Ray rOrigin rDirectory) distance = rOrigin + multvs rDirectory distance
