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

module RaytracaH.Light where

import Data.Vec

import RaytracaH.Color
import RaytracaH.Math

-- TODO: use light color in calculations
data Light = Directional Vector3D Float (Color Float)

lightDir :: Light -> Vector3D
lightDir (Directional dir _ _) = dir

lightIntensity :: Light -> Float
lightIntensity (Directional _ intensity _) = intensity

lightColor :: Light -> Color Float
lightColor (Directional _ _ color) = color

data LightFactors = LightFactors {
    diffuseFactor :: Float,
    specularFactor :: Float
}

sumFactors :: LightFactors -> LightFactors -> LightFactors
sumFactors (LightFactors d1 s1) (LightFactors d2 s2) = LightFactors (d1 + d2) (s1 + s2)
