module Primitive where

import Ray

data IntersectionResult = Intersection Float | NoIntersection deriving (Show, Eq)

class Primitive a where
    intersect :: a -> Ray -> IntersectionResult
