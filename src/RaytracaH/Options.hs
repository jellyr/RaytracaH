module RaytracaH.Options where

import RaytracaH.Color

data RayTracerOptions = RayTracerOptions {
    imgWidth :: Int,
    imgHeight :: Int,
    backgroundColor :: Color Float,
    infinityDistance :: Float,
    shadowBias :: Float
}