module Material where

import Color

data Material = DiffusiveMaterial (Color Float) | DiffusiveAndSpecularMaterial (Color Float) Int

materialColor :: Material -> Color Float
materialColor (DiffusiveMaterial c) = c
materialColor (DiffusiveAndSpecularMaterial c _) = c