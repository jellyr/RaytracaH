module Plane where

import Data.Vec (dot)

import Color
import Primitive
import Ray
import Util

data Plane = Plane {
    point :: Vector3D,
    normal :: Vector3D,
    color :: Color Int
}

instance Primitive Plane where
    intersect (Plane planePoint planeNormal _) (Ray rayOrigin rayDir) = 
        if distance >= 0 then
            Intersection distance
        else 
            NoIntersection
        where
            distance = ((planePoint - rayOrigin) `dot` planeNormal) / (planeNormal `dot` rayDir)

    normalAtHitPoint (Plane _ planeNormal _) _ =
        planeNormal

    color (Plane _ _ planeColor) = 
        planeColor
