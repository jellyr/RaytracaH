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

module RaytracaH.Plane.Test where

import Test.QuickCheck

import qualified Data.Vec as Vec

import RaytracaH.Color
import RaytracaH.Material 
import RaytracaH.Math
import RaytracaH.Plane
import RaytracaH.Primitive.Test
import RaytracaH.Primitive
import RaytracaH.Ray

raysDirectedAtPlane :: Plane -> Gen Ray
raysDirectedAtPlane (Plane planePoint planeNormal _) = do
    rayOrigin <- arbitrary :: (Gen AnyVector3D)
    return $ Ray (v3d rayOrigin) (Vec.normalize $ planePoint - v3d rayOrigin)

prop_raysDirectedAtPlaneAlwaysHit :: Plane -> Property
prop_raysDirectedAtPlaneAlwaysHit plane =
    forAll (raysDirectedAtPlane plane) $ \ray ->
        rayHitPrimitive plane ray

prop_raysNotDirectedAtPlaneAlwaysMiss :: Plane -> Property
prop_raysNotDirectedAtPlaneAlwaysMiss plane =
    forAll (raysDirectedAtPlane plane) $ \ray ->
        rayMissPrimitive plane (Ray (origin ray) (- (direction ray)))
