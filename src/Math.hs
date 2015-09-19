module Math where

import Test.QuickCheck (Arbitrary(..), choose)

import Data.Vec

type Vector2D = Packed (Vec2 Float)
type Vector3D = Packed (Vec3 Float)

deg2rad :: Float -> Float
deg2rad deg = deg * pi / 180.0

rad2deg :: Float -> Float
rad2deg rad = 180.0 * rad / pi

multvs :: Vector3D -> Float -> Vector3D
multvs v scalar = v * Vec3F scalar scalar scalar

newtype AnyVector3D = AnyVector3D { v3d :: Vector3D } deriving (Eq, Show)

instance Arbitrary AnyVector3D where
    arbitrary = 
        AnyVector3D <$> v
        where
            v = Vec3F <$> choose (1.0, 10.0) <*> choose (1.0, 10.0) <*> choose (1.0, 10.0)