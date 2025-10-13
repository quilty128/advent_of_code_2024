module Shared.VectorHelper where

import qualified Data.Vector as V
import Prelude (flip, ($), (-))

tails :: V.Vector a -> V.Vector (V.Vector a)
tails vec = V.map ((flip V.drop) vec) $ V.fromList [0 .. V.length vec - 1]
