module Shared.VectorHelper where

import qualified Data.Vector as V

tails :: V.Vector a -> V.Vector (V.Vector a)
tails vec = V.map ((flip V.drop) vec) (V.fromList [ 0 .. length vec - 1 ])
