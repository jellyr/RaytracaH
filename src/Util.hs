module Util where

import Data.Vec

type Vector2D = Packed (Vec2 Float)
type Vector3D = Packed (Vec3 Float)

deg2rad :: Float -> Float
deg2rad deg = deg * pi / 180.0

rad2deg :: Float -> Float
rad2deg rad = 180.0 * rad / pi

multvs :: Vector3D -> Float -> Vector3D
multvs v scalar = v * Vec3F scalar scalar scalar
