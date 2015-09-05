module Primitive where

import qualified Data.Vec as Vec

import Ray
import Util

data IntersectionResult = Intersection Float | NoIntersection deriving (Show, Eq)

class Primitive a where
    intersect :: a -> Ray -> IntersectionResult

data Sphere = Sphere {
    center :: Vector3D,
    r :: Float
}

-- TODO: write it in more readable way
instance Primitive Sphere where
    intersect (Sphere c rr) (Ray rayOrigin rayDir) = 
        if tca < 0 || d2 > rr2 || all (< 0) tParams then
            NoIntersection
        else
            Intersection (minimum (filter (> 0) tParams))
        where
            vecL = c - rayOrigin
            tca = Vec.dot vecL rayDir
            d2 = Vec.dot vecL vecL - tca * tca
            rr2 = rr * rr
            thc = sqrt (rr2 - d2)
            t0 = tca - thc
            t1 = tca + thc
            tParams = [t0, t1]
