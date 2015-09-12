module Scene where

import qualified Data.Vector as V

import Light
import Primitive

data Scene = Scene {
    sceneLights :: V.Vector Light,
    sceneObjects :: V.Vector AnyPrimitive
}
