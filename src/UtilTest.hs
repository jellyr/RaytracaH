module Util.Test where

import Test.QuickCheck

import Util

prop_deg2rad :: Float -> Bool
prop_deg2rad deg = deg2rad deg == deg * pi / 180.0

prop_rad2deg :: Float -> Bool
prop_rad2deg rad = rad2deg rad == 180.0 * rad / pi

main :: IO ()
main = do
    quickCheck prop_deg2rad
    quickCheck prop_rad2deg