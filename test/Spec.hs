import Test.QuickCheck
import Test.Hspec

import qualified RaytracaH.Math.Test as MT
import qualified RaytracaH.Ray.Test as RT
import qualified RaytracaH.Sphere.Test as ST

main :: IO ()
main = do
    quickCheck MT.prop_deg2rad
    quickCheck MT.prop_rad2deg
    quickCheck MT.prop_mutlvs
    quickCheck RT.prop_allPrimaryRaysDirectionNormalized
    quickCheck ST.prop_hitPointAtRadiusDistance
    quickCheck ST.prop_rayDirectedAtSphereIntersect
