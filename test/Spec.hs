import Test.QuickCheck
import Test.Hspec

import MathTest as MT
import RayTest as RT

main :: IO ()
main = do
    quickCheck MT.prop_deg2rad
    quickCheck MT.prop_rad2deg
    quickCheck MT.prop_mutlvs
    quickCheck RT.prop_allPrimaryRaysDirectionNormalized

