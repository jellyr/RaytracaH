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