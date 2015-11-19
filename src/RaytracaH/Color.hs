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

{-# LANGUAGE OverloadedStrings #-}

module RaytracaH.Color where

import Control.Applicative
import Data.Aeson

data Color a = Color a a a deriving (Show)

instance Functor Color where
    fmap f (Color rr gg bb) = Color (f rr) (f gg) (f bb)

instance Applicative Color where
    pure f = Color f f f
    (Color f g h) <*> (Color a b c) = Color (f a) (g b) (h c)

sumColors :: (Num a, Ord a) => a -> Color a -> Color a -> Color a
sumColors limit a b = (\x y -> min limit (x + y)) <$> a <*> b

instance ToJSON a => ToJSON (Color a) where
    toJSON (Color r g b) =
        object [ "r" .= r
               , "g" .= g
               , "b" .= b]

instance FromJSON a => FromJSON (Color a) where
    parseJSON (Object o) =
        Color <$> o .: "r" <*> o .: "g" <*> o .: "b"
    parseJSON _ =
        empty
