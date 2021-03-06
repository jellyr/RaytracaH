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

module RaytracaH.Screen where

import Test.QuickCheck (Arbitrary(..), choose)

data Screen = Screen {
    width :: Int,
    height :: Int
} deriving (Show)

instance Arbitrary Screen where
    arbitrary = do
        w <- choose (1, 100)
        h <- choose (1, 100)
        return $ Screen w h