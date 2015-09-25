{-

Copyright 2015 Rafał Nowak

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

-}

module RaytracaH.Color where

import RaytracaH.Bitmap(Pixel(..))

data Color a = Color a a a deriving (Show)

instance Functor Color where
    fmap f (Color rr gg bb) = Color (f rr) (f gg) (f bb)

toPixel :: Color Float -> Pixel
toPixel (Color rr gg bb) = Pixel (componentToPixelRange rr) (componentToPixelRange gg) (componentToPixelRange bb)
    where
        componentToPixelRange c = ceiling $ c * 255.0

sumColors :: (Num a, Ord a) => a -> Color a -> Color a -> Color a
sumColors limit (Color rA gA bA) (Color rB gB bB) = Color r g b
    where
        r = min limit (rA + rB)
        g = min limit (gA + gB)
        b = min limit (bA + bB)
