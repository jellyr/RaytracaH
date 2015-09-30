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

module RaytracaH.Math.Test where

import Data.Vec

import RaytracaH.Math

prop_deg2rad :: Float -> Bool
prop_deg2rad deg = 
    deg2rad deg == deg * pi / 180.0

prop_rad2deg :: Float -> Bool
prop_rad2deg rad = 
    rad2deg rad == 180.0 * rad / pi

prop_mutlvs :: Float -> Float -> Float -> Float -> Bool
prop_mutlvs x y z s = 
    multvs (Vec3F x y z) s == Vec3F (x*s) (y*s) (z*s)

prop_clampedToPositiveBiggerOrEqualZero :: Float -> Bool
prop_clampedToPositiveBiggerOrEqualZero x = 
    clampedToPositive x >= 0.0

prop_limitedToOneAllLessThanOrEqualToOne :: Float -> Bool
prop_limitedToOneAllLessThanOrEqualToOne x =
    limitedToOne x <= 1.0

prop_angleOfIncidenceEqualToAngleOfReflection :: AnyVector3D -> AnyVector3D -> Bool
prop_angleOfIncidenceEqualToAngleOfReflection incidenceVector normal = 
    equalsWithEpsilon angleOfIncidence angleOfReflection
    where
        vec' = v3d incidenceVector
        normal' = normalize $ v3d normal
        reflection = reflect vec' normal'
        angleOfIncidence = acos $ vec' `dot` normal' / (norm vec' * norm normal')
        angleOfReflection = acos $ (-reflection) `dot` normal' / (norm reflection * norm normal')
