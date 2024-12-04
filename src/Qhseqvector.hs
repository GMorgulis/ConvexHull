{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Qhseqvector
    (V2, VV2)
    where

{-
George Morgulis 
COMS 4995 Parallel Functional Programming

This is the sequential implementaion of quickhull
-}


import Data.List (maximumBy, minimumBy, nub)
import Data.Ord (comparing)
import qualified Data.Vector as V


{-New vector type-}
type V2 = V.Vector Double
type VV2 = V.Vector V2
type VV3 = V.Vector VV2

--------------------------------------------------------------------------------------------

-- Function to find the V2 with the maximum x-coordinate
maxv :: VV2 -> V2
maxv = V.maximumBy (comparing (V.! 0))

-- Function to find the V2 with the maximum x-coordinate
minv :: VV2 -> V2
minv = V.maximumBy (comparing (V.! 0))

---------------------------------------------------------------------------------------------



keepOuter :: V2 -> V2 -> V2 -> VV2 -> VV2
keepOuter t1 t2 t3 = V.filter (not . pointInTriangle t1 t2 t3)

pointInTriangle :: V2 -> V2 -> V2 -> V2 -> Bool
pointInTriangle t1 t2 t3 p =
    closeEnough (triArea t1 t2 p + triArea t1 t3 p + triArea t2 t3 p) (triArea t1 t2 t3)

triArea :: V2 -> V2 -> V2 -> Double
triArea v1 v2 v3 =
    let x1 = v1 V.! 0
        y1 = v1 V.! 1
        x2 = v2 V.! 0
        y2 = v2 V.! 1
        x3 = v3 V.! 0
        y3 = v3 V.! 1
    in abs ((x1 * (y2 - y3)) + (x2 * (y3 - y1)) + (x3 * (y1 - y2)))













{-Epsilon value to mitigate floating point error-}
epsilon :: Double
epsilon = 1e-9

{-Method to mitigate floating point error-}
closeEnough :: Double -> Double -> Bool
closeEnough a b = abs (a - b) < epsilon