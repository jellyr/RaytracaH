module Ray.Test where

import Test.QuickCheck

import RayTracer
import Ray
import Data.Vec as Vec
import Data.Vector as V

epsilon :: Float
epsilon = 1e-5

prop_allPrimaryRaysDirectionNormalized :: Screen -> Float -> Bool
prop_allPrimaryRaysDirectionNormalized screen fov =
    V.all (\(Ray _ directon) -> abs (Vec.norm directon - 1.0) < epsilon) rays
    where
        eye = Vec.Vec3F 0 0 0
        rays = generatePrimaryRays screen fov eye

main :: IO ()
main = quickCheck prop_allPrimaryRaysDirectionNormalized