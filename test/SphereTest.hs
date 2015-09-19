module SphereTest where

import Test.QuickCheck

import qualified Data.Vec as Vec

import Color
import Material 
import Math
import Primitive
import Ray
import Sphere

raysOutsideSphere :: Sphere -> Gen Ray
raysOutsideSphere sphere = do
    xDistance <- choose (sphereRadius, sphereRadius * 2.0)
    yDistance <- choose (sphereRadius, sphereRadius * 2.0)
    zDistance <- choose (sphereRadius, sphereRadius * 2.0)
    rayDirection <- arbitrary :: (Gen AnyVector3D)
    return $ Ray (sphereCenter + Vec.Vec3F xDistance yDistance zDistance) (v3d rayDirection)
    where
        sphereCenter = center sphere
        sphereRadius = radius sphere

prop_hitPointInRadiusDistance :: Sphere -> Property
prop_hitPointInRadiusDistance sphere = 
    forAll (raysOutsideSphere sphere) $ \ray ->
        rayHitPointInRadius sphere ray
        
rayHitPointInRadius :: Sphere -> Ray -> Bool
rayHitPointInRadius sphere ray = 
    case intersection of Intersection distance ->
                             let
                                hitPoint = pointOnRay ray distance
                                sphereCenter = center sphere
                                sphereRadius = radius sphere
                             in
                                equalsWithEpsilon (Vec.norm $ sphereCenter - hitPoint) sphereRadius
                         _ ->
                             True
    where
        intersection = intersect sphere ray