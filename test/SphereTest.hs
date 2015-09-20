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
    xDistance <- choose (sphereRadius * 1.2, sphereRadius * 2.0)
    yDistance <- choose (sphereRadius * 1.2, sphereRadius * 2.0)
    zDistance <- choose (sphereRadius * 1.2, sphereRadius * 2.0)
    rayDirection <- arbitrary :: (Gen AnyVector3D)
    return $ Ray (sphereCenter + Vec.Vec3F xDistance yDistance zDistance) (v3d rayDirection)
    where
        sphereCenter = center sphere
        sphereRadius = radius sphere

prop_hitPointAtRadiusDistance :: Sphere -> Property
prop_hitPointAtRadiusDistance sphere = 
    forAll (raysOutsideSphere sphere) $ \ray ->
        rayHitPointAtRadius sphere ray
        
rayHitPointAtRadius :: Sphere -> Ray -> Bool
rayHitPointAtRadius sphere ray = 
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

raysDirectedAtSphere :: Sphere -> Gen Ray
raysDirectedAtSphere sphere = do
    (Ray rayOrigin _) <- raysOutsideSphere sphere
    return $ Ray rayOrigin (Vec.normalize $ center sphere - rayOrigin)

prop_rayDirectedAtSphereIntersect :: Sphere -> Property
prop_rayDirectedAtSphereIntersect sphere = 
    forAll (raysDirectedAtSphere sphere) $ \ray ->
        rayHitSphere sphere ray

rayHitSphere :: Sphere -> Ray -> Bool
rayHitSphere sphere ray =
    case intersection of Intersection _ -> True
                         _ -> False
    where
        intersection = intersect sphere ray
