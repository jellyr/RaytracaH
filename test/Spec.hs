import Test.QuickCheck

import RayTest as RT
import UtilTest as UT

main :: IO ()
main = do
    quickCheck RT.prop_allPrimaryRaysDirectionNormalized
    quickCheck UT.prop_deg2rad
    quickCheck UT.prop_rad2deg
    quickCheck UT.prop_mutlvs