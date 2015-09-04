module Primitive where

import Ray

class Primitive a where
    intersect :: a -> Ray -> Bool
