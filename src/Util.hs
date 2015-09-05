module Util where

import qualified Data.Vec as Vec

type Vector2D = Vec.Packed (Vec.Vec2 Float)
type Vector3D = Vec.Packed (Vec.Vec3 Float)

deg2rad :: Float -> Float
deg2rad deg = deg * pi / 180.0

rad2deg :: Float -> Float
rad2deg rad = 180.0 * rad / pi
