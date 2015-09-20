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

module RaytracaH.Math where

import Test.QuickCheck (Arbitrary(..), choose)

import Data.Vec

type Vector2D = Packed (Vec2 Float)
type Vector3D = Packed (Vec3 Float)

deg2rad :: Float -> Float
deg2rad deg = deg * pi / 180.0

rad2deg :: Float -> Float
rad2deg rad = 180.0 * rad / pi

multvs :: Vector3D -> Float -> Vector3D
multvs v scalar = v * Vec3F scalar scalar scalar

reflect :: Vector3D -> Vector3D -> Vector3D
reflect i n = i - (multvs n (2 * (dot i n)))

comparisonEpsilon :: Fractional a => a
comparisonEpsilon = 1e-5

equalsWithEpsilon :: Float -> Float -> Bool
equalsWithEpsilon a b = 
    (abs (a - b)) < comparisonEpsilon

newtype AnyVector3D = AnyVector3D { v3d :: Vector3D } deriving (Eq, Show)

instance Arbitrary AnyVector3D where
    arbitrary = 
        AnyVector3D <$> v
        where
            v = Vec3F <$> choose (1.0, 100.0) <*> choose (1.0, 100.0) <*> choose (1.0, 100.0)