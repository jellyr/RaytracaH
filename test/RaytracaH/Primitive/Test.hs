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

module RaytracaH.Primitive.Test where

import Test.QuickCheck

import qualified Data.Vec as Vec

import RaytracaH.Math
import RaytracaH.Primitive
import RaytracaH.Ray

rayHitPrimitive :: Primitive -> Ray -> Bool
rayHitPrimitive primitive ray =
    case intersection of Intersection _ -> True
                         NoIntersection -> False
    where
        intersection = primitive `intersect` ray

rayMissPrimitive :: Primitive -> Ray -> Bool
rayMissPrimitive primitive ray = not $ rayHitPrimitive primitive ray

raysDirectedAtPlane :: Primitive -> Gen Ray
raysDirectedAtPlane (Plane planePoint _ _) = do
    rayOrigin <- arbitrary :: (Gen AnyVector3D)
    return $ Ray (v3d rayOrigin) (Vec.normalize $ planePoint - v3d rayOrigin)
raysDirectedAtPlane _ = undefined

prop_raysDirectedAtPlaneAlwaysHit :: Property
prop_raysDirectedAtPlaneAlwaysHit =
    forAll arbitraryPlane $ \plane ->
        forAll (raysDirectedAtPlane plane) $ \ray ->
            rayHitPrimitive plane ray

prop_raysNotDirectedAtPlaneAlwaysMiss :: Property
prop_raysNotDirectedAtPlaneAlwaysMiss =
    forAll arbitraryPlane $ \plane ->
        forAll (raysDirectedAtPlane plane) $ \ray ->
            rayMissPrimitive plane (Ray (origin ray) (- (direction ray)))

raysOutsideSphere :: Primitive -> Gen Ray
raysOutsideSphere (Sphere center radius _) = do
    xDistance <- choose (radius * 1.2, radius * 2.0)
    yDistance <- choose (radius * 1.2, radius * 2.0)
    zDistance <- choose (radius * 1.2, radius * 2.0)
    rayDirection <- arbitrary :: (Gen AnyVector3D)
    return $ Ray (center + Vec.Vec3F xDistance yDistance zDistance) (Vec.normalize $ v3d rayDirection)
raysOutsideSphere _ = undefined

prop_hitPointAtRadiusDistance :: Property
prop_hitPointAtRadiusDistance = 
    forAll arbitrarySphere $ \sphere ->
        forAll (raysOutsideSphere sphere) $ \ray ->
            rayHitPointAtRadius sphere ray

rayHitPointAtRadius :: Primitive -> Ray -> Bool
rayHitPointAtRadius sphere@(Sphere center radius _) ray = 
    case intersection of Intersection distance ->
                             let
                                hitPoint = pointOnRay ray distance
                             in
                                equalsCustomEpsilon 1e-4 (Vec.norm $ center - hitPoint) radius
                         _ ->
                             True
    where
        intersection = sphere `intersect` ray
rayHitPointAtRadius _ _ = undefined

raysDirectedAtSphere :: Primitive -> Gen Ray
raysDirectedAtSphere sphere@(Sphere center _ _) = do
    (Ray rayOrigin _) <- raysOutsideSphere sphere
    return $ Ray rayOrigin (Vec.normalize $ center - rayOrigin)
raysDirectedAtSphere _ = undefined

prop_rayDirectedAtSphereIntersect :: Property
prop_rayDirectedAtSphereIntersect =
    forAll arbitrarySphere $ \sphere ->
        forAll (raysDirectedAtSphere sphere) $ \ray ->
            rayHitPrimitive sphere ray
