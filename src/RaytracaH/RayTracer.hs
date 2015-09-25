{-

Copyright 2015 Rafał Nowak

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

-}

module RaytracaH.RayTracer where

import RaytracaH.Bitmap
import RaytracaH.Camera
import RaytracaH.Color
import RaytracaH.Light
import RaytracaH.Material
import RaytracaH.Math
import RaytracaH.Options
import RaytracaH.Primitive
import RaytracaH.Ray
import RaytracaH.Scene
import RaytracaH.Screen

import qualified Data.Vec as Vec
import qualified Data.Vector as V

data PrimitiveIntersection = IntersectionWithPrimitive AnyPrimitive IntersectionResult |
                             NoIntersectionWithPrimitive

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
        V.map (toPixel . traceRay options scene 1) primaryRays
    where
        primaryRays = generatePrimaryRays screen camera

traceRay :: RayTracerOptions -> Scene -> Int -> Ray -> Color Float
traceRay options scene depth ray
    | depth >= 5 = backgroundColor options
    | otherwise = case primitiveWithIntersection of IntersectionWithPrimitive hitPrimitive (Intersection hitDistance) -> 
                                                        traceRayBasedOnMaterial options scene depth ray hitPrimitive hitDistance
                                                    _ -> 
                                                        backgroundColor options
                  where
                      primitiveWithIntersection = findNearestIntersectingPrimitive (sceneObjects scene) ray (infinityDistance options)

traceRayBasedOnMaterial :: RayTracerOptions -> Scene -> Int -> Ray -> AnyPrimitive -> Float -> Color Float
traceRayBasedOnMaterial options scene prevDepth ray hitPrimitive hitDistance =
    case 
        material hitPrimitive 
    of ReflectiveMaterial _ kR ->
           traceRayForReflectiveSurface options scene prevDepth ray hitPrimitive hitDistance kR
       _ ->
           sumLightsEffect options scene hitPrimitive (rayHitPoint ray hitDistance) (direction ray)

traceRayForReflectiveSurface :: RayTracerOptions -> Scene -> Int -> Ray -> AnyPrimitive -> Float -> Float -> Color Float
traceRayForReflectiveSurface options scene prevDepth originalRay hitPrimitive hitDistance kR =
    let
        hitPoint = rayHitPoint originalRay hitDistance
        nHit = normalAtHitPoint hitPrimitive hitPoint
        reflectRayOrigin = hitPoint + multvs nHit (shadowBias options)
        reflectRayDirection = reflect (direction originalRay) nHit
        colorFromReflections = 
            (\col -> kR * col) <$> traceRay options scene (prevDepth + 1) (Ray reflectRayOrigin reflectRayDirection)
        lightsColor = 
            sumLightsEffect options scene hitPrimitive (rayHitPoint originalRay hitDistance) (direction originalRay)
    in
        sumColors 1.0 colorFromReflections lightsColor

rayHitPoint :: Ray -> Float -> Vector3D
rayHitPoint = pointOnRay

sumLightsEffect :: RayTracerOptions -> Scene -> AnyPrimitive -> Vector3D -> Vector3D -> Color Float
sumLightsEffect options scene hitPrimitive hitPoint rayDirection = 
    fmap (\c -> limitedToOne (diffuse * c + specular)) (materialColor $ material hitPrimitive)
    where
        LightFactors diffuse specular = phongFactors options scene hitPrimitive hitPoint rayDirection

phongFactors :: RayTracerOptions -> Scene -> AnyPrimitive -> Vector3D -> Vector3D -> LightFactors
phongFactors options scene hitPrimitive hitPoint rayDirection = 
    V.foldl' (\previousLightFactors light ->
        let
            lightFactors = phongFactorForLight options (sceneObjects scene) light hitPrimitive hitPoint rayDirection
        in
            sumFactors lightFactors previousLightFactors
    ) (LightFactors 0.0 0.0) (sceneLights scene)

isHitPrimitiveInShadow :: RayTracerOptions -> V.Vector AnyPrimitive -> Light -> AnyPrimitive -> Vector3D -> Bool
isHitPrimitiveInShadow options primitives light hitPrimitive hitPoint = 
    case intersectionWithShadowRay of IntersectionWithPrimitive _ _ -> True
                                      _ -> False
    where
        shadowRay = createShadowRay options light hitPrimitive hitPoint
        intersectionWithShadowRay = findNearestIntersectingPrimitive primitives shadowRay (infinityDistance options)

createShadowRay :: RayTracerOptions -> Light -> AnyPrimitive -> Vector3D -> Ray
createShadowRay options light hitPrimitive hitPoint = 
    Ray (hitPoint + multvs (normalAtHitPoint hitPrimitive hitPoint) (shadowBias options)) (-vecL)
    where
        vecL = lightDir light

findNearestIntersectingPrimitive :: V.Vector AnyPrimitive -> Ray -> Float -> PrimitiveIntersection
findNearestIntersectingPrimitive primitives ray lastNearestDistance = 
    findNearestIntersectingPrimitiveIter primitives ray lastNearestDistance NoIntersectionWithPrimitive

findNearestIntersectingPrimitiveIter :: V.Vector AnyPrimitive -> Ray -> Float -> PrimitiveIntersection -> PrimitiveIntersection
findNearestIntersectingPrimitiveIter primitives ray lastNearestDistance result
    | V.null primitives = result
    | otherwise =
        case intersection of NoIntersection -> 
                                 proceedWithNoIntersection
                             Intersection distance -> 
                                 if distance < lastNearestDistance then 
                                     findInTail distance (IntersectionWithPrimitive currentPrimitive intersection)
                                 else
                                     proceedWithNoIntersection
        where
            currentPrimitive = V.head primitives
            intersection = currentPrimitive `intersect` ray
            findInTail = findNearestIntersectingPrimitiveIter (V.tail primitives) ray
            proceedWithNoIntersection = findInTail lastNearestDistance result

phongFactorForLight :: RayTracerOptions -> V.Vector AnyPrimitive -> Light -> AnyPrimitive -> Vector3D -> Vector3D -> LightFactors
phongFactorForLight options objectsInScene light hitPrimitive hitPoint rayDirection =
    if isHitPrimitiveInShadow options objectsInScene light hitPrimitive hitPoint then
        LightFactors 0.0 0.0
    else
        case material hitPrimitive of DiffusiveMaterial _ ->
                                          LightFactors (limitedToOne diffuse) 0.0
                                      DiffusiveAndSpecularMaterial _ _ ->
                                          LightFactors (limitedToOne diffuse) (limitedToOne specular)
                                      ReflectiveMaterial _ _ ->
                                          LightFactors (limitedToOne diffuse) 0.0
    where
        diffuse = calculateDiffuseForHitPrimitive light hitPrimitive hitPoint
        specular = calculateSpecularForHitPrimitive light hitPrimitive hitPoint rayDirection

calculateDiffuseForHitPrimitive :: Light -> AnyPrimitive -> Vector3D -> Float
calculateDiffuseForHitPrimitive light hitPrimitive hitPoint =
    diffuse
    where
        nHit = normalAtHitPoint hitPrimitive hitPoint
        diffuse = lightIntensity light * clampedToPositive (Vec.dot nHit (-1.0 * lightDir light))

calculateSpecularForHitPrimitive :: Light -> AnyPrimitive -> Vector3D -> Vector3D -> Float
calculateSpecularForHitPrimitive light hitPrimitive hitPoint rayDirection =
    specular
    where
        nHit = normalAtHitPoint hitPrimitive hitPoint
        reflectedVec = reflect (lightDir light) nHit
        n = case material hitPrimitive of DiffusiveAndSpecularMaterial _ specularExponent -> 
                                              clampedToPositive (Vec.dot reflectedVec (-1.0 * rayDirection)) ** specularExponent
                                          _ -> 
                                              0.0
        specular = lightIntensity light * n
