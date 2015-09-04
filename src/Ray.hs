module Ray(
        Ray(..)
    ) where

import Data.Vec

data Ray = Ray {
    origin :: Vec3 Float,
    direction :: Vec3 Float
}
