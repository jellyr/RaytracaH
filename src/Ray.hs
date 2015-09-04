module Ray(
        Ray(..),
        generatePrimaryRays
    ) where

import qualified Data.Vec as Vec
import qualified Data.Vector as V

import RayTracer

data Ray = Ray {
    origin :: Vec.Vec3 Float,
    direction :: Vec.Vec3 Float
}

instance Show Ray where
    show (Ray originOf directionOf) = "Ray: " ++ show originOf ++ ", " ++ show directionOf

generatePrimaryRays :: Screen -> Float -> Vec.Vec3 Float -> V.Vector Ray
generatePrimaryRays screen fov eye = undefined
