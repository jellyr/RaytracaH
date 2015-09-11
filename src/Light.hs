module Light where

import Data.Vec

import Color
import Util

data Light = Directional Vector3D (Color Float)
