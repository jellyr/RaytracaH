module Sphere where

import Test.QuickCheck (Gen(..), Arbitrary(..), choose)

import Data.Vec (dot, normalize)

import Color
import Material
import Math
import Primitive
import Ray

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