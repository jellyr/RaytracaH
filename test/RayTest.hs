module RayTest where

import Test.QuickCheck

import Camera
import Ray
import Screen
import Data.Vec as Vec
import Data.Vector as V

epsilon :: Float
epsilon = 1e-5

prop_allPrimaryRaysDirectionNormalized :: Screen -> Float -> Bool
prop_allPrimaryRaysDirectionNormalized screen fov =
    V.all (\(Ray _ directon) -> abs (Vec.norm directon - 1.0) < epsilon) rays
    where
        camera = Camera (Vec.Vec3F 0.0 0.0 10.0) (Vec.Vec3F 0.0 0.0 0.0) (Vec.Vec3F 0.0 1.0 0.0)
        rays = generatePrimaryRays screen fov camera
