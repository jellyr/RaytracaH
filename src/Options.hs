module Options where

import Color

data RayTracerOptions = RayTracerOptions {
    imgWidth :: Int,
    imgHeight :: Int,
    backgroundColor :: Color Float,
    infinityDistance :: Float,
    shadowBias :: Float
}