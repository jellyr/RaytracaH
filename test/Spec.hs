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

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import qualified RaytracaH.Math.Test as MT
import qualified RaytracaH.Light.Test as LT
import qualified RaytracaH.Primitive.Test as PT
import qualified RaytracaH.Ray.Test as RT

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
    [ QC.testProperty "deg2rad" MT.prop_deg2rad
    , QC.testProperty "rad2deg" MT.prop_rad2deg
    , QC.testProperty "pmutlvs" MT.prop_mutlvs
    , QC.testProperty "clampedToPositiveBiggerOrEqualZero" MT.prop_clampedToPositiveBiggerOrEqualZero
    , QC.testProperty "limitedToOneAllLessThanOrEqualToOne" MT.prop_limitedToOneAllLessThanOrEqualToOne
    , QC.testProperty "angleOfIncidenceEqualToAngleOfReflection" MT.prop_angleOfIncidenceEqualToAngleOfReflection
    , QC.testProperty "intensityForDirectionalLightIsConstant" LT.prop_intensityForDirectionalLightIsConstant
    , QC.testProperty "intensityForPointLightDecreasesWithDistance" LT.prop_intensityForPointLightDecreasesWithDistance
    , QC.testProperty "raysDirectedAtPlaneAlwaysHit" PT.prop_raysDirectedAtPlaneAlwaysHit
    , QC.testProperty "raysNotDirectedAtPlaneAlwaysMiss" PT.prop_raysNotDirectedAtPlaneAlwaysMiss
    , QC.testProperty "hitPointAtRadiusDistance" PT.prop_hitPointAtRadiusDistance
    , QC.testProperty "rayDirectedAtSphereIntersect" PT.prop_rayDirectedAtSphereIntersect
    , QC.testProperty "allPrimaryRaysDirectionNormalized" RT.prop_allPrimaryRaysDirectionNormalized
    ]
