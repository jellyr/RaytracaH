module RayTracer where

data Screen = Screen {
    width :: Int,
    height :: Int
}

instance Show Screen where
    show screen = "Screen: " ++ show (width screen) ++ "x" ++ show (height screen)
