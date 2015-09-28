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

module RaytracaH.Camera where

import Data.Vec

import RaytracaH.Math

data Camera = Camera {
    position :: Vector3D,
    target :: Vector3D,
    up :: Vector3D,
    fov :: Float
}

pointToCameraSpace :: Camera -> Vector3D -> Vector3D
pointToCameraSpace camera v =
    Vec3F (x/w) (y/w) (z/w)
    where
        view = rotationLookAt (unpack $ up camera) (unpack $ position camera) (unpack $ target camera)
        (x :. y :. z :. w :. ()) = multmv view (snoc (unpack v) 1.0)
