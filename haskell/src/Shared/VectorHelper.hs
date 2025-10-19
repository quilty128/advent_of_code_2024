module Shared.VectorHelper where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude (flip, (+))

tails :: Vector a -> Vector (Vector a)
tails vec = V.generate (V.length vec + 1) ((flip V.drop) vec)
