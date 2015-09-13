module Material where

import Color

data Material = DiffusiveMaterial (Color Int) | DiffusiveAndSpecularMaterial (Color Int) Int

materialColor :: Material -> Color Int
materialColor (DiffusiveMaterial c) = c
materialColor (DiffusiveAndSpecularMaterial c _) = c