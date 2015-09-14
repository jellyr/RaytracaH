module Material where

import Color

data Material = DiffusiveMaterial (Color Float) | DiffusiveAndSpecularMaterial (Color Float) Float | ReflectiveMaterial

materialColor :: Material -> Color Float
materialColor (DiffusiveMaterial c) = c
materialColor (DiffusiveAndSpecularMaterial c _) = c
materialColor ReflectiveMaterial = Color 0.0 0.0 0.0