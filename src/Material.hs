module Material where

import Color

-- TODO: make composable materials
data Material = DiffusiveMaterial (Color Float) | 
    DiffusiveAndSpecularMaterial (Color Float) Float | 
    ReflectiveMaterial (Color Float) Float deriving (Show)

materialColor :: Material -> Color Float
materialColor (DiffusiveMaterial c) = c
materialColor (DiffusiveAndSpecularMaterial c _) = c
materialColor (ReflectiveMaterial c _) = c