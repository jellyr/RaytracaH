module Option where

import Color

data RayTracerOptions = RayTracerOptions {
    imgWidth :: Int,
    imgHeight :: Int,
    backgroundColor :: Color Int,
    infinityDistance :: Float,
    shadowBias :: Float
}