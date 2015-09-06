module Primitive where

import Bitmap(Pixel(..))
import Ray (Ray)
import Util

data IntersectionResult = Intersection Float | NoIntersection deriving (Show, Eq)

data Color a = Color a a a

instance Functor Color where
    fmap f (Color rr gg bb) = Color (f rr) (f gg) (f bb)

toPixel :: Color Int -> Pixel
toPixel (Color rr gg bb) = Pixel rr gg bb

class Primitive a where
    intersect :: a -> Ray -> IntersectionResult
    normalAtHitPoint :: a -> Vector3D -> Vector3D
    color :: a -> Color Int
