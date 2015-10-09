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

{-# LANGUAGE DeriveGeneric #-}

module RaytracaH.Primitive where

import Test.QuickCheck (Arbitrary(..), Gen, choose)

import Data.Aeson
import Data.Vec (dot, normalize)
import GHC.Generics

import RaytracaH.Color
import RaytracaH.Material
import RaytracaH.Math
import RaytracaH.Ray (Ray(..))

data Primitive = Plane {
    planePoint :: Vector3D,
    planeNormal :: Vector3D,
    material :: Material } | Sphere {
    center :: Vector3D,
    radius :: Float,
    material :: Material } deriving (Show, Generic)

instance ToJSON Primitive
instance FromJSON Primitive

data IntersectionResult = Intersection Float | NoIntersection deriving (Show, Eq)

data PrimitiveIntersection = IntersectionWithPrimitive Primitive IntersectionResult |
                             NoIntersectionWithPrimitive

intersect :: Primitive -> Ray -> IntersectionResult
intersect (Plane pPoint pNormal _) (Ray rayOrigin rayDir) = 
    if distance >= 0 then
        Intersection distance
    else
        NoIntersection
    where
        distance = ((pPoint - rayOrigin) `dot` pNormal) / (pNormal `dot` rayDir)
intersect (Sphere sphereCenter sphereRadius _) (Ray rayOrigin rayDir) =
    if tca < 0 || dSquared > rSquared || all (< 0.0) distanceParams || any isNaN distanceParams then
        NoIntersection
    else
        Intersection (minimum (filter (>= 0.0) distanceParams))
    where
        vecL = sphereCenter - rayOrigin
        tca = vecL `dot` rayDir
        dSquared = vecL `dot` vecL - tca * tca
        rSquared = sphereRadius * sphereRadius
        thc = sqrt (rSquared - dSquared)
        distanceParams = [tca - thc, tca + thc]

normalAtHitPoint :: Primitive -> Vector3D -> Vector3D
normalAtHitPoint (Plane _ normal _) _ = normal
normalAtHitPoint (Sphere center _ _) hitPoint = normalize (hitPoint - center)

arbitrarySphere :: Gen Primitive
arbitrarySphere = do
    sphereCenter <- arbitrary :: (Gen AnyVector3D)
    sphereRadius <- choose (1.0, 100.0)
    return $ Sphere (v3d sphereCenter) sphereRadius (Material (Color 1.0 1.0 1.0) Nothing Nothing)

arbitraryPlane :: Gen Primitive
arbitraryPlane = do
    arbitraryPoint <- arbitrary :: (Gen AnyVector3D)
    arbitraryNormal <- arbitrary :: (Gen AnyVector3D)
    return $ Plane (v3d arbitraryPoint) (normalize $ v3d arbitraryNormal) (Material (Color 1.0 1.0 1.0) Nothing Nothing)
