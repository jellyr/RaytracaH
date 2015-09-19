import Test.QuickCheck
import Test.Hspec

import qualified MathTest as MT
import qualified RayTest as RT
import qualified SphereTest as ST

main :: IO ()
main = do
    quickCheck MT.prop_deg2rad
    quickCheck MT.prop_rad2deg
    quickCheck MT.prop_mutlvs
    quickCheck RT.prop_allPrimaryRaysDirectionNormalized
    quickCheck ST.prop_hitPointInRadiusDistance
