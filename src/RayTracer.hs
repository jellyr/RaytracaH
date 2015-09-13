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

data PrimitiveIntersection = PrimitiveIntersection (Maybe AnyPrimitive) IntersectionResult

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
                                          sumLightsEffect options scene hitPrimitive (rayHitPoint ray hitDistance) (direction ray)
                                      _ -> 
                                          toPixel $ backgroundColor options
    where
        primitiveWithintersection = findNearestIntersectingPrimitive (sceneObjects scene) ray (infinityDistance options)

rayHitPoint :: Ray -> Float -> Vector3D
rayHitPoint (Ray rOrigin rDirectory) distance = rOrigin + multvs rDirectory distance

sumLightsEffect :: RayTracerOptions -> Scene -> AnyPrimitive -> Vector3D -> Vector3D -> Pixel
sumLightsEffect options scene hitPrimitive hitPoint rayDirection = 
    toPixel $ fmap (\c -> min 1.0 (diffuse * c + specular)) (materialColor $ material hitPrimitive)
    where
        (diffuse, specular) = phongFactors options scene hitPrimitive hitPoint rayDirection

-- TODO: refactor it into smaller functions
phongFactors :: RayTracerOptions -> Scene -> AnyPrimitive -> Vector3D -> Vector3D -> (Float, Float)
phongFactors options scene hitPrimitive hitPoint rayDirection = 
    V.foldl (\(diffuse, specular) light ->
        if isHitPrimitiveInShadow options (sceneObjects scene) light hitPrimitive hitPoint == True then
            (diffuse, specular)
        else
            case material hitPrimitive of DiffusiveMaterial _ ->
                                              (min 1.0 (diffuse + calculateDiffuseForHitPrimitive light hitPrimitive hitPoint), specular)
                                          DiffusiveAndSpecularMaterial _ _ ->
                                              (min 1.0 (diffuse + calculateDiffuseForHitPrimitive light hitPrimitive hitPoint), min 1.0 (specular + calculateSpecularForHitPrimitive light hitPrimitive hitPoint rayDirection))
    ) (0.0, 0.0) (sceneLights scene)

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

findNearestIntersectingPrimitive :: V.Vector AnyPrimitive -> Ray -> Float -> PrimitiveIntersection
findNearestIntersectingPrimitive primitives ray distanceNearest = 
    findNearestIntersectingPrimitiveIter primitives ray distanceNearest (PrimitiveIntersection Nothing NoIntersection)

findNearestIntersectingPrimitiveIter :: V.Vector AnyPrimitive -> Ray -> Float -> PrimitiveIntersection -> PrimitiveIntersection
findNearestIntersectingPrimitiveIter primitives ray distanceNearest result
    | V.null primitives = result
    | otherwise = 
        case intersection of NoIntersection -> 
                                 proceedWithNoIntersection
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

calculateSpecularForHitPrimitive :: Light -> AnyPrimitive -> Vector3D -> Vector3D -> Float
calculateSpecularForHitPrimitive light hitPrimitive hitPoint rayDirection =
    specular
    where
        nHit = normalAtHitPoint hitPrimitive hitPoint
        rVec = reflect (lightDir light) nHit
        n = case material hitPrimitive of DiffusiveAndSpecularMaterial _ n -> 
                                              (max 0.0 (Vec.dot rVec (-1.0 * rayDirection))) ** n
                                          _ -> 
                                              0.0
        specular = lightIntensity light * n

reflect :: Vector3D -> Vector3D -> Vector3D
reflect i n = i - (multvs n (2 * (Vec.dot i n)))
