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
