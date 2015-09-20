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

module RaytracaH.Plane where

import Data.Vec (dot)

import RaytracaH.Material
import RaytracaH.Math
import RaytracaH.Primitive
import RaytracaH.Ray

data Plane = Plane {
    point :: Vector3D,
    normal :: Vector3D,
    material :: Material
}

instance Primitive Plane where
    intersect (Plane planePoint planeNormal _) (Ray rayOrigin rayDir) = 
        if distance >= 0 then
            Intersection distance
        else 
            NoIntersection
        where
            distance = ((planePoint - rayOrigin) `dot` planeNormal) / (planeNormal `dot` rayDir)

    normalAtHitPoint (Plane _ planeNormal _) _ =
        planeNormal

    material (Plane _ _ planeMaterial) = 
        planeMaterial