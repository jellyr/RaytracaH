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

module RaytracaH.Sphere where

import Test.QuickCheck (Arbitrary(..), Gen(..), choose)

import Data.Vec (dot, normalize)

import RaytracaH.Color
import RaytracaH.Material
import RaytracaH.Math
import RaytracaH.Primitive
import RaytracaH.Ray

data Sphere = Sphere {
    center :: Vector3D,
    radius :: Float,
    material :: Material
} deriving (Show)

instance Primitive Sphere where
    intersect (Sphere sphereCenter sphereRadius _) (Ray rayOrigin rayDir) = 
        if tca < 0 || dSquared > rSquared || all (< 0.0) distanceParams || any (isNaN) distanceParams then
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

    normalAtHitPoint (Sphere sphereCenter _ _) hitPoint =
        normalize (hitPoint - sphereCenter)

    material (Sphere _ _ sphereMaterial) =
        sphereMaterial

instance Arbitrary Sphere where
    arbitrary = do
        sphereCenter <- arbitrary :: (Gen AnyVector3D)
        sphereRadius <- choose (1.0, 100.0)
        return $ Sphere (v3d sphereCenter) sphereRadius (DiffusiveMaterial (Color 1.0 1.0 1.0))
