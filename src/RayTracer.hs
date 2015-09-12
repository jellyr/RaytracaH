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

fileWithRenderedImage :: Int -> Int -> V.Vector AnyPrimitive -> Light -> Camera -> PPMFile
fileWithRenderedImage screenW screenH primitives light camera = 
    PPMFile (PPMFileHeader screenW screenH 255) (render screen primitives light camera)
    where
        screen = Screen screenW screenH

backgroundColor :: Pixel
backgroundColor = Pixel 194 204 255

shadowColor :: Pixel
shadowColor = Pixel 0 0 0

infinityDistance :: Float
infinityDistance = 10000.0

render :: Screen -> V.Vector AnyPrimitive -> Light -> Camera -> Pixels
render screen primitives light camera
    | V.null primitives = V.map (const backgroundColor) primaryRays
    | otherwise = 
        V.map (traceRay primitives light) primaryRays
    where
        primaryRays = generatePrimaryRays screen camera

-- TODO: calculate color in more elegant way, take into account light color
traceRay :: V.Vector AnyPrimitive -> Light -> Ray -> Pixel
traceRay primitives light ray = 
    case primitiveWithintersection of PrimitiveIntersection (Just hitPrimitive) (Intersection hitDistance) -> 
                                          if isHitPrimitiveInShadow primitives light hitPrimitive ray hitDistance == True then
                                              shadowColor
                                          else
                                              calculateColorForHitPrimitive hitDistance hitPrimitive ray
                                      _ -> backgroundColor
    where
        primitiveWithintersection = findNearestIntersectingPrimitive primitives ray infinityDistance

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

calculateColorForHitPrimitive :: Float -> AnyPrimitive -> Ray -> Pixel
calculateColorForHitPrimitive distance primitive ray@(Ray rOrigin rDirectory) = 
    let
        pHit = rayHitPoint ray distance
        nHit = normalAtHitPoint primitive pHit
        intensity = max 0.0 (Vec.dot nHit (rDirectory * (-1.0)))
        primitiveColor = fmap (\c -> ceiling (intensity * fromIntegral c)) (color primitive)
    in
        toPixel primitiveColor

rayHitPoint :: Ray -> Float -> Vector3D
rayHitPoint (Ray rOrigin rDirectory) distance = rOrigin + multvs rDirectory distance
