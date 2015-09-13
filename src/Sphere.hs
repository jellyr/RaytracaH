module Sphere where

import Data.Vec (dot, normalize)

import Material
import Primitive
import Ray
import Util

data Sphere = Sphere {
    center :: Vector3D,
    radius :: Float,
    material :: Material
}

instance Primitive Sphere where
    intersect (Sphere sphereCenter sphereRadius _) (Ray rayOrigin rayDir) = 
        if tca < 0 || dSquared > rSquared || all (< 0) distanceParams then
            NoIntersection
        else
            Intersection (minimum (filter (> 0) distanceParams))
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
