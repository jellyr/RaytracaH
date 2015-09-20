{-

Copyright 2015 Rafał Nowak

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

-}

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
