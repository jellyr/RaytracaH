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
    intensity <- arbitrary :: Gen Float
    return $ Directional (v3d direction) intensity

pointLights :: Gen Light
pointLights = do
    position <- vectorWithTermsInRange 1.0 100.0
    intensity <- choose (0.1, 50.0) :: Gen Float
    return $ Point (v3d position) intensity

prop_intensityForDirectionalLightIsConstant :: Property
prop_intensityForDirectionalLightIsConstant =
    forAll (vectorWithTermsInRange 1.0 100.0) $ \point ->
        forAll directionalLights $ \light@(Directional _ intensity) ->
            lightIntensityInPoint (v3d point) light == intensity

prop_intensityForPointLightDecreasesWithDistance :: Property
prop_intensityForPointLightDecreasesWithDistance = 
    forAll pointLights $ \light ->
        forAll (vectorWithTermsInRange 1.0 100.0) $ \point1 ->
            let
                point1' = v3d point1
                lightDir = lightDirection point1' light
                point2 = point1' + multvs lightDir 10.0
            in
                lightIntensityInPoint point2 light < lightIntensityInPoint point1' light
