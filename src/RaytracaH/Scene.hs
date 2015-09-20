module RaytracaH.Scene where

import qualified Data.Vector as V

import RaytracaH.Light
import RaytracaH.Primitive

data Scene = Scene {
    sceneLights :: V.Vector Light,
    sceneObjects :: V.Vector AnyPrimitive
}
