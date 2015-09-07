{-# LANGUAGE ExistentialQuantification #-}

module Primitive where

import Color
import Ray (Ray)
import Util

data IntersectionResult = Intersection Float | NoIntersection deriving (Show, Eq)

class Primitive a where
    intersect :: a -> Ray -> IntersectionResult
    normalAtHitPoint :: a -> Vector3D -> Vector3D
    color :: a -> Color Int

-- TODO: eliminate existential type
data AnyPrimitive = forall p . Primitive p => AnyPrimitive p

instance Primitive AnyPrimitive where 
    intersect (AnyPrimitive p) = intersect p
    normalAtHitPoint (AnyPrimitive p) = normalAtHitPoint p
    color (AnyPrimitive p) = color p
