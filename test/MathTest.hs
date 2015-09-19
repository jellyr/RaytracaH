module MathTest where

import Test.QuickCheck

import Data.Vec

import Math

prop_deg2rad :: Float -> Bool
prop_deg2rad deg = 
    deg2rad deg == deg * pi / 180.0

prop_rad2deg :: Float -> Bool
prop_rad2deg rad = 
    rad2deg rad == 180.0 * rad / pi

prop_mutlvs :: Float -> Float -> Float -> Float -> Bool
prop_mutlvs x y z s = 
    multvs (Vec3F x y z) s == (Vec3F (x*s) (y*s) (z*s))
