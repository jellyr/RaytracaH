module Primitive where

import Color
import Ray (Ray)
import Util

data IntersectionResult = Intersection Float | NoIntersection deriving (Show, Eq)

class Primitive a where
    intersect :: a -> Ray -> IntersectionResult
    normalAtHitPoint :: a -> Vector3D -> Vector3D
    color :: a -> Color Int
