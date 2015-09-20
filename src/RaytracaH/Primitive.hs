{-# LANGUAGE ExistentialQuantification #-}

module RaytracaH.Primitive where

import RaytracaH.Material
import RaytracaH.Math
import RaytracaH.Ray (Ray)

data IntersectionResult = Intersection Float | NoIntersection deriving (Show, Eq)

class Primitive a where
    intersect :: a -> Ray -> IntersectionResult
    normalAtHitPoint :: a -> Vector3D -> Vector3D
    material :: a -> Material

-- TODO: eliminate existential type
data AnyPrimitive = forall p . Primitive p => AnyPrimitive p

instance Primitive AnyPrimitive where 
    intersect (AnyPrimitive p) = intersect p
    normalAtHitPoint (AnyPrimitive p) = normalAtHitPoint p
    material (AnyPrimitive p) = material p
