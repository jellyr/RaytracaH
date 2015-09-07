module Color where

import Bitmap(Pixel(..))

data Color a = Color a a a

instance Functor Color where
    fmap f (Color rr gg bb) = Color (f rr) (f gg) (f bb)

toPixel :: Color Int -> Pixel
toPixel (Color rr gg bb) = Pixel rr gg bb