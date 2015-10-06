{-

Copyright 2015 Rafał Nowak

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

-}

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import Control.Monad (unless)
import System.Exit (exitFailure)

import qualified RaytracaH.Math.Test as MT
import qualified RaytracaH.Light.Test as LT
import qualified RaytracaH.Plane.Test as PT
import qualified RaytracaH.Ray.Test as RT
import qualified RaytracaH.Sphere.Test as ST

main :: IO ()
main = do
    let tests = [ quickCheckResult MT.prop_deg2rad
                , quickCheckResult MT.prop_rad2deg
                , quickCheckResult MT.prop_mutlvs
                , quickCheckResult MT.prop_clampedToPositiveBiggerOrEqualZero
                , quickCheckResult MT.prop_limitedToOneAllLessThanOrEqualToOne
                , quickCheckResult MT.prop_angleOfIncidenceEqualToAngleOfReflection
                , quickCheckResult LT.prop_intensityForDirectionalLightIsConstant
                , quickCheckResult LT.prop_intensityForPointLightDecreasesWithDistance
                , quickCheckResult PT.prop_raysDirectedAtPlaneAlwaysHit
                , quickCheckResult PT.prop_raysNotDirectedAtPlaneAlwaysMiss
                , quickCheckResult RT.prop_allPrimaryRaysDirectionNormalized
                , quickCheckResult ST.prop_hitPointAtRadiusDistance
                , quickCheckResult ST.prop_rayDirectedAtSphereIntersect
                ]
    success <- fmap (all isSuccess) (sequence tests)
    unless success exitFailure
