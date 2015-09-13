module RayTracer where

import Bitmap
import Camera
import Color
import Light
import Material
import Options
import Primitive
import Ray
import Scene
import Screen
import Util

import qualified Data.Vec as Vec
import qualified Data.Vector as V

data PrimitiveIntersection a = PrimitiveIntersection (Maybe a) IntersectionResult

fileWithRenderedImage :: Camera -> RayTracerOptions -> Scene -> PPMFile
fileWithRenderedImage camera options scene = 
    PPMFile (PPMFileHeader screenW screenH 255) (render screen camera options scene)
    where
        screenW = imgWidth options
        screenH = imgHeight options
        screen = Screen screenW screenH

render :: Screen -> Camera -> RayTracerOptions -> Scene -> Pixels
render screen camera options scene
    | V.null (sceneObjects scene) = V.map (const $ toPixel (backgroundColor options)) primaryRays
    | otherwise = 
        V.map (traceRay options scene) primaryRays
    where
        primaryRays = generatePrimaryRays screen camera

traceRay :: RayTracerOptions -> Scene -> Ray -> Pixel
traceRay options scene ray = 
    case primitiveWithintersection of PrimitiveIntersection (Just hitPrimitive) (Intersection hitDistance) -> 
                                          sumLightsEffect options scene hitPrimitive (rayHitPoint ray hitDistance)
                                      _ -> 
                                          toPixel $ backgroundColor options
    where
        primitiveWithintersection = findNearestIntersectingPrimitive (sceneObjects scene) ray (infinityDistance options)

rayHitPoint :: Ray -> Float -> Vector3D
rayHitPoint (Ray rOrigin rDirectory) distance = rOrigin + multvs rDirectory distance

sumLightsEffect :: RayTracerOptions -> Scene -> AnyPrimitive -> Vector3D -> Pixel
sumLightsEffect options scene hitPrimitive hitPoint = 
    toPixel $ fmap (\c -> ceiling (diffuse * fromIntegral c)) (materialColor $ material hitPrimitive)
    where
        diffuse = min 1.0 (V.sum $ V.map (\light ->
                if isHitPrimitiveInShadow options (sceneObjects scene) light hitPrimitive hitPoint == True then
                    0.0
                else
                    calculateDiffuseForHitPrimitive light hitPrimitive hitPoint
            ) (sceneLights scene))

isHitPrimitiveInShadow :: RayTracerOptions -> V.Vector AnyPrimitive -> Light -> AnyPrimitive -> Vector3D -> Bool
isHitPrimitiveInShadow options primitives light hitPrimitive hitPoint = 
    let
        shadowRay = createShadowRay options light hitPrimitive hitPoint
        primitiveWithintersection = findNearestIntersectingPrimitive primitives shadowRay (infinityDistance options)
    in
        case primitiveWithintersection of PrimitiveIntersection _ (Intersection _) -> True
                                          _ -> False

createShadowRay :: RayTracerOptions -> Light -> AnyPrimitive -> Vector3D -> Ray
createShadowRay options light hitPrimitive hitPoint = 
    Ray (hitPoint + multvs nHit (shadowBias options)) (-vecL)
    where
        nHit = normalAtHitPoint hitPrimitive hitPoint
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

calculateDiffuseForHitPrimitive :: Light -> AnyPrimitive -> Vector3D -> Float
calculateDiffuseForHitPrimitive light hitPrimitive hitPoint =
    diffuse
    where
        nHit = normalAtHitPoint hitPrimitive hitPoint
        diffuse = lightIntensity light * max 0.0 (Vec.dot nHit (-1.0 * lightDir light))
