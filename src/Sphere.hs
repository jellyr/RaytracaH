module Sphere where

import Data.Vec (dot)

import Primitive
import Ray
import Util

data Sphere = Sphere {
    center :: Vector3D,
    r :: Float
}

instance Primitive Sphere where
    intersect (Sphere c rr) (Ray rayOrigin rayDir) = 
        if tca < 0 || dSquared > rSquared || all (< 0) tParams then
            NoIntersection
        else
            Intersection (minimum (filter (> 0) tParams))
        where
            vecL = c - rayOrigin
            tca = vecL `dot` rayDir
            dSquared = vecL `dot` vecL - tca * tca
            rSquared = rr * rr
            thc = sqrt (rSquared - dSquared)
            tParams = [tca - thc, tca + thc]
