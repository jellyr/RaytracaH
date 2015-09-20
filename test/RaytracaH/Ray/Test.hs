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

module RaytracaH.Ray.Test where

import Test.QuickCheck

import Data.Vec as Vec
import Data.Vector as V

import RaytracaH.Camera
import RaytracaH.Math
import RaytracaH.Ray
import RaytracaH.Screen

prop_allPrimaryRaysDirectionNormalized :: Screen -> Float -> Bool
prop_allPrimaryRaysDirectionNormalized screen fov =
    V.all (\ray -> equalsWithEpsilon (Vec.norm $ direction ray) 1.0) rays
    where
        camera = Camera (Vec.Vec3F 0.0 0.0 10.0) (Vec.Vec3F 0.0 0.0 0.0) (Vec.Vec3F 0.0 1.0 0.0) fov
        rays = generatePrimaryRays screen camera
