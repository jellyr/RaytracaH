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

{-# LANGUAGE DeriveGeneric #-}

module RaytracaH.Light where

import Data.Aeson
import Data.Vec
import GHC.Generics

import RaytracaH.Math

-- TODO: use light color in calculations
data Light = Directional {
    dir :: Vector3D,
    intensity :: Float } | Point {
    pos :: Vector3D,
    intensity :: Float } deriving (Show, Generic)

instance ToJSON Light
instance FromJSON Light

lightIntensityInPoint :: Vector3D -> Light -> Float
lightIntensityInPoint _ (Directional _ lightIntensity) = lightIntensity
lightIntensityInPoint point (Point position lightIntensity) =
    lightIntensity / attenuation
    where
        lightDirUnnormalized = point - position
        attenuation = 4 * pi * norm lightDirUnnormalized

lightDirection :: Vector3D -> Light -> Vector3D
lightDirection _ (Directional direction _) = direction
lightDirection point (Point position _) =
    normalize $ point - position

data LightFactors = LightFactors {
    diffuse :: Float,
    specular :: Float
}

sumFactors :: LightFactors -> LightFactors -> LightFactors
sumFactors (LightFactors d1 s1) (LightFactors d2 s2) = LightFactors (d1 + d2) (s1 + s2)
