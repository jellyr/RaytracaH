module RaytracaH.Color where

import RaytracaH.Bitmap(Pixel(..))

data Color a = Color a a a deriving (Show)

instance Functor Color where
    fmap f (Color rr gg bb) = Color (f rr) (f gg) (f bb)

toPixel :: Color Float -> Pixel
toPixel (Color rr gg bb) = Pixel (componentToPixelRange rr) (componentToPixelRange gg) (componentToPixelRange bb)
    where
        componentToPixelRange c = ceiling $ c * 255.0

-- TODO: replace 1.0 with some nice maximum value from color and make this function polymorphic
sumColors :: Color Float -> Color Float -> Color Float
sumColors (Color rA gA bA) (Color rB gB bB) = Color r g b
    where
        r = min 1.0 (rA + rB)
        g = min 1.0 (gA + gB)
        b = min 1.0 (bA + bB)
