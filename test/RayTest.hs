module RayTest where

import Test.QuickCheck

import Camera
import Math
import Ray
import Screen
import Data.Vec as Vec
import Data.Vector as V

prop_allPrimaryRaysDirectionNormalized :: Screen -> Float -> Bool
prop_allPrimaryRaysDirectionNormalized screen fov =
    V.all (\ray -> equalsWithEpsilon (Vec.norm $ direction ray) 1.0) rays
    where
        camera = Camera (Vec.Vec3F 0.0 0.0 10.0) (Vec.Vec3F 0.0 0.0 0.0) (Vec.Vec3F 0.0 1.0 0.0) fov
        rays = generatePrimaryRays screen camera
