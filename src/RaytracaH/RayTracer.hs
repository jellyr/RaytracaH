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

import Control.Applicative

import RaytracaH.Bitmap
import RaytracaH.Camera
import RaytracaH.Color
import qualified RaytracaH.Light as Light
import qualified RaytracaH.Material as Material
import RaytracaH.Math
import RaytracaH.Options
import RaytracaH.Primitive
import qualified RaytracaH.Ray as Ray
import qualified RaytracaH.Scene as Scene
import RaytracaH.Screen

import qualified Data.Vec as Vec
import qualified Data.Vector as V

fileWithRender :: RayTracerOptions -> Scene.SceneWithCamera -> PPMFile
fileWithRender options (Scene.SceneWithCamera scene camera) =
    PPMFile (PPMFileHeader screenW screenH 255) (render options scene screen camera)
    where
        screenW = imgWidth options
        screenH = imgHeight options
        screen = Screen screenW screenH

render :: RayTracerOptions -> Scene.Scene -> Screen -> Camera -> Pixels
render options scene screen camera
    | V.null (Scene.objects scene) = V.map (const $ fromColor (backgroundColor options)) primaryRays
    | otherwise =
        V.map (fromColor . traceRay options scene 1) primaryRays
    where
        primaryRays = Ray.generatePrimaryRays screen camera

traceRay :: RayTracerOptions -> Scene.Scene -> Int -> Ray.Ray -> Color Float
traceRay options scene depth ray
    | depth >= 5 = backgroundColor options
    | otherwise = case primitiveWithIntersection of IntersectionWithPrimitive hitPrimitive (Intersection hitDistance) ->
                                                        traceRayBasedOnMaterial options scene depth ray hitPrimitive hitDistance
                                                    _ ->
                                                        backgroundColor options
                  where
                      primitiveWithIntersection = findNearestIntersectingPrimitive (Scene.objects scene) ray (infinityDistance options)

traceRayBasedOnMaterial :: RayTracerOptions -> Scene.Scene -> Int -> Ray.Ray -> Primitive -> Float -> Color Float
traceRayBasedOnMaterial options scene prevDepth ray hitPrimitive hitDistance =
    case
        material hitPrimitive
    of Material.Material _ _ (Just kR) ->
           traceRayForReflectiveSurface options scene prevDepth ray hitPrimitive hitDistance kR
       _ ->
           sumLightsEffect options scene hitPrimitive (rayHitPoint ray hitDistance) (Ray.direction ray)

traceRayForReflectiveSurface :: RayTracerOptions -> Scene.Scene -> Int -> Ray.Ray -> Primitive -> Float -> Float -> Color Float
traceRayForReflectiveSurface options scene prevDepth originalRay hitPrimitive hitDistance kR =
    let
        hitPoint = rayHitPoint originalRay hitDistance
        nHit = normalAtHitPoint hitPrimitive hitPoint
        reflectRayOrigin = hitPoint + multvs nHit (shadowBias options)
        reflectRayDirection = reflect (Ray.direction originalRay) nHit
        colorFromReflections =
            liftA (\col -> kR * col) (traceRay options scene (prevDepth + 1) (Ray.Ray reflectRayOrigin reflectRayDirection))
        lightsColor =
            sumLightsEffect options scene hitPrimitive (rayHitPoint originalRay hitDistance) (Ray.direction originalRay)
    in
        sumColors 1.0 colorFromReflections lightsColor

rayHitPoint :: Ray.Ray -> Float -> Vector3D
rayHitPoint = Ray.pointOnRay

sumLightsEffect :: RayTracerOptions -> Scene.Scene -> Primitive -> Vector3D -> Vector3D -> Color Float
sumLightsEffect options scene hitPrimitive hitPoint rayDirection =
    liftA (\c -> limitedToOne (diffuse * c + specular)) (Material.color (material hitPrimitive))
    where
        Light.LightFactors diffuse specular = phongFactors options scene hitPrimitive hitPoint rayDirection

phongFactors :: RayTracerOptions -> Scene.Scene -> Primitive -> Vector3D -> Vector3D -> Light.LightFactors
phongFactors options scene hitPrimitive hitPoint rayDirection =
    V.foldl' (\previousLightFactors light ->
        let
            lightFactors = phongFactorForLight options (Scene.objects scene) light hitPrimitive hitPoint rayDirection
        in
            Light.sumFactors lightFactors previousLightFactors
    ) (Light.LightFactors 0.0 0.0) (Scene.lights scene)

isHitPrimitiveInShadow :: RayTracerOptions -> V.Vector Primitive -> Light.Light -> Primitive -> Vector3D -> Bool
isHitPrimitiveInShadow options primitives light hitPrimitive hitPoint =
    case intersectionWithShadowRay of IntersectionWithPrimitive _ _ -> True
                                      _ -> False
    where
        shadowRay = createShadowRay options light hitPrimitive hitPoint
        intersectionWithShadowRay = findNearestIntersectingPrimitive primitives shadowRay (infinityDistance options)

createShadowRay :: RayTracerOptions -> Light.Light -> Primitive -> Vector3D -> Ray.Ray
createShadowRay options light hitPrimitive hitPoint =
    Ray.Ray (hitPoint + multvs (normalAtHitPoint hitPrimitive hitPoint) (shadowBias options)) (-vecL)
    where
        vecL = Light.lightDirection hitPoint light

findNearestIntersectingPrimitive :: V.Vector Primitive -> Ray.Ray -> Float -> PrimitiveIntersection
findNearestIntersectingPrimitive primitives ray lastNearestDistance =
    findNearestIntersectingPrimitiveIter primitives ray lastNearestDistance NoIntersectionWithPrimitive

findNearestIntersectingPrimitiveIter :: V.Vector Primitive -> Ray.Ray -> Float -> PrimitiveIntersection -> PrimitiveIntersection
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

phongFactorForLight :: RayTracerOptions -> V.Vector Primitive -> Light.Light -> Primitive -> Vector3D -> Vector3D -> Light.LightFactors
phongFactorForLight options objectsInScene light hitPrimitive hitPoint rayDirection =
    if isHitPrimitiveInShadow options objectsInScene light hitPrimitive hitPoint then
        Light.LightFactors 0.0 0.0
    else
        case material hitPrimitive of Material.Material _ Nothing _ ->
                                          Light.LightFactors (limitedToOne diffuse) 0.0
                                      Material.Material _ (Just _) _ ->
                                          Light.LightFactors (limitedToOne diffuse) (limitedToOne specular)
    where
        diffuse = calculateDiffuseForHitPrimitive light hitPrimitive hitPoint
        specular = calculateSpecularForHitPrimitive light hitPrimitive hitPoint rayDirection

calculateDiffuseForHitPrimitive :: Light.Light -> Primitive -> Vector3D -> Float
calculateDiffuseForHitPrimitive light hitPrimitive hitPoint =
    diffuse
    where
        nHit = normalAtHitPoint hitPrimitive hitPoint
        lightIntensity = Light.lightIntensityInPoint hitPoint light
        diffuse = lightIntensity * clampedToPositive (Vec.dot nHit (-1.0 * Light.lightDirection hitPoint light))

calculateSpecularForHitPrimitive :: Light.Light -> Primitive -> Vector3D -> Vector3D -> Float
calculateSpecularForHitPrimitive light hitPrimitive hitPoint =
    specularForMaterialAndReflection lightIntensity (material hitPrimitive) reflectedVec
    where
        nHit = normalAtHitPoint hitPrimitive hitPoint
        reflectedVec = reflect (Light.lightDirection hitPoint light) nHit
        lightIntensity = Light.lightIntensityInPoint hitPoint light

specularForMaterialAndReflection :: Float -> Material.Material -> Vector3D -> Vector3D -> Float
specularForMaterialAndReflection lightIntensity primitiveMaterial reflectedVec rayDirection =
    case primitiveMaterial of
        Material.Material _ (Just kS) _ ->
            lightIntensity * (clampedToPositive (Vec.dot reflectedVec (-1.0 * rayDirection)) ** kS)
        _ ->
            0.0
