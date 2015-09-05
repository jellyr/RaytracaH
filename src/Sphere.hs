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
        if tca < 0 || d2 > rr2 || all (< 0) tParams then
            NoIntersection
        else
            Intersection (minimum (filter (> 0) tParams))
        where
            vecL = c - rayOrigin
            tca = vecL `dot` rayDir
            d2 = vecL `dot` vecL - tca * tca
            rr2 = rr * rr
            thc = sqrt (rr2 - d2)
            tParams = [tca - thc, tca + thc]
