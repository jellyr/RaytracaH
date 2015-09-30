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

module RaytracaH.Light.Test where

import Test.QuickCheck

import RaytracaH.Light
import RaytracaH.Math

directionalLights :: Gen Light
directionalLights = do
    direction <- vectorWithTermsInRange 1.0 100.0
    color <- arbitrary :: Gen Float
    return $ Directional (v3d direction) color

prop_intensityForDirectionalLightIsConstant :: Property
prop_intensityForDirectionalLightIsConstant =
    forAll (vectorWithTermsInRange 1.0 100.0) $ \point ->
        forAll directionalLights $ \light@(Directional _ intensity) ->
            lightIntensityInPoint (v3d point) light == intensity
