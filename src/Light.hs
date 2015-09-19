module Light where

import Data.Vec

import Color
import Math

-- TODO: use light color in calculations
data Light = Directional Vector3D Float (Color Float)

lightDir :: Light -> Vector3D
lightDir (Directional dir _ _) = dir

lightIntensity :: Light -> Float
lightIntensity (Directional _ intensity _) = intensity

lightColor :: Light -> Color Float
lightColor (Directional _ _ color) = color
