module Plane where

import Data.Vec (dot)

import Color
import Primitive
import Ray
import Util

data Plane = Plane {
    p :: Vector3D,
    normal :: Vector3D,
    color :: Color Int
}

instance Primitive Plane where
    intersect (Plane pp n _) (Ray rayOrigin rayDir) = 
        if t >= 0 then
            Intersection t
        else 
            NoIntersection
        where
            t = ((pp - rayOrigin) `dot` n) / (n `dot` rayDir)

    normalAtHitPoint (Plane _ planeNormal _) _ =
        planeNormal

    color (Plane _ _ planeColor) = 
        planeColor
