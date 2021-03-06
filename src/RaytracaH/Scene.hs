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

module RaytracaH.Scene where

import Data.Aeson
import qualified Data.Vector as V
import GHC.Generics

import RaytracaH.Camera
import RaytracaH.Light
import RaytracaH.Primitive

data Scene = Scene {
    lights :: V.Vector Light,
    objects :: V.Vector Primitive
} deriving (Show, Generic)

instance ToJSON Scene
instance FromJSON Scene

data SceneWithCamera = SceneWithCamera {
    scene :: Scene,
    camera :: Camera } deriving (Show, Generic)

instance ToJSON SceneWithCamera
instance FromJSON SceneWithCamera

