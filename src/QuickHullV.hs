module QuickHullV (V2, VV2, quickh, maxv, minv, maxAreaPoint, grouper) where

{-
George Morgulis 
COMS 4995 Parallel Functional Programming

This is the paralel implementaion of quickhull which is actually good
-}


import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as V
import Control.Parallel (par, pseq)


{-New vector type-}
type V2 = (Double, Double)
type VV2 = V.Vector V2

--------------------------------------------------------------------------------------------
quickh :: VV2 -> Int -> VV2
quickh points d = V.cons a1 (V.cons a2 hpar)
  where
    a1 = minv points
    a2 = maxv points
    h1 = ph points a1 a2 (depthUpdate d)
    h2 = ph points a2 a1 (depthUpdate d) -- note the diffrence in order
    hpar =  h1 `par` (h2 `pseq` V.concat [h1, h2]) 

ph :: VV2 -> V2 -> V2 -> Int -> VV2
ph points a1 a2 d
  | V.length group == 0 = V.empty 
  | d > 0 && V.length group > 250000 = V.cons m1 hpar
  | otherwise = V.cons m1 h
  where
    group = grouper a1 a2 points
    m1 = maxAreaPoint a1 a2 group
    h1 = ph group a1 m1 (depthUpdate d)
    h2 = ph group m1 a2 (depthUpdate d)
    h = V.concat [h1, h2]
    hpar = h1 `par` (h2 `pseq` V.concat [h1, h2]) 

{-Function to find the V2 with the maximum x-coordinate-}
maxv :: VV2 -> V2
maxv points | V.null points = error "Error: maxv requires an Unboxed Vector of points but none been provided. Points are (double, double)"
maxv points = V.maximumBy (comparing fst) points

{-Function to find the V2 with the maximum x-coordinate-}
minv :: VV2 -> V2
minv points | V.null points = error "Error: minv requires an Unboxed Vector of points but none been provided. Points are (double, double)"
minv points = V.minimumBy (comparing fst) points

{-Furthest point from a line-}
maxAreaPoint :: V2 -> V2 -> VV2 -> V2
maxAreaPoint _ _ points | V.null points = error "Error: maxAreaPoint requires an Unboxed Vector of points but none been provided. Points are (double, double)"
maxAreaPoint anchor1 anchor2 points = V.maximumBy (comparing (triArea anchor1 anchor2)) points

{-Groups by determinant (left, right)-}
grouper :: V2 -> V2 -> VV2 -> VV2
grouper anchor1 anchor2 points = V.filter (\z -> determinant anchor1 anchor2 z > 0) points

{-Calculates determinant-}
determinant :: V2 -> V2 -> V2 -> Double
determinant (z1, z2) (w1, w2) (u1, u2) = (w1 - z1) * (u2 - z2) - (w2 - z2) * (u1 - z1)

{-Area for triangle-}
triArea :: V2 -> V2 -> V2 -> Double
triArea (z1, z2) (w1, w2) (u1, u2) = abs ((z1 * (w2 - u2)) + (w1 * (u2 - z2)) + (u1 * (z2 - w2)))

depthUpdate :: Int -> Int
depthUpdate d
  | d <= 1 = 0
  | otherwise = div d 2