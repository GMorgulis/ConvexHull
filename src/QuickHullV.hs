module QuickHullV (V2, VV2, quickh, maxv, minv, maxAreaPoint, grouper) where

{-
George Morgulis 
COMS 4995 Parallel Functional Programming

This is the paralel implementaion of quickhull
-}


import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Split as VS
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies(parMap, rpar, rdeepseq)
import Control.DeepSeq
--Control.Parallel.Strategies (parList, rseq, using)


{-New vector type-}
type V2 = (Double, Double)
type VV2 = V.Vector V2

--------------------------------------------------------------------------------------------

quickh :: VV2 -> Int -> VV2
quickh points n = V.cons a1 (V.cons a2 hh)
  where
    a1 = minv points
    a2 = maxv points
    hh = starter points a1 a2 n

starter :: VV2 -> V2 -> V2 -> Int -> VV2
starter points a1 a2 d = V.cons m1 (V.cons m2 h) 
  where 
      group1 = fst (grouper a1 a2 points) 
      group2 = snd (grouper a1 a2 points) 
      m1 = maxAreaPoint a1 a2 group1 
      m2 = maxAreaPoint a1 a2 group2
      h1 = ph group1 a1 m1 V.empty d 
      h2 = ph group1 m1 a2 V.empty d 
      h3 = ph group2 a2 m2 V.empty d
      h4 = ph group2 m2 a1 V.empty d 
      h = h1 V.++ h2 V.++ h3 V.++ h4 

ph :: VV2 -> V2 -> V2 -> VV2-> Int -> VV2 
ph points a1 a2 hull d 
  | V.null points = V.empty 
  | otherwise = V.cons m1 h
  where 
    group = fst (grouper a1 a2 points)
    m1 = maxAreaPoint a1 a2 group
    h1 = ph group a1 m1 hull d 
    h2 = ph group m1 a2 hull d 
    h = h1 V.++ h2


-------------------------------------------------------------------------------------------------------

{-Function to find the V2 with the maximum x-coordinate-}
maxv :: VV2 -> V2
maxv = V.maximumBy (comparing fst)

{-Function to find the V2 with the maximum x-coordinate-}
minv :: VV2 -> V2
minv = V.minimumBy (comparing fst)

--------------------------------------------------------------------------------------------------------

{-Furthest point from a line-}
maxAreaPoint :: V2 -> V2 -> VV2 -> V2
maxAreaPoint _ anchor2 points | V.null points = anchor2
maxAreaPoint anchor1 anchor2 points = V.maximumBy (comparing (triArea anchor1 anchor2)) points

{-Groups by determinant (left, right)-}
grouper :: V2 -> V2 -> VV2 -> (VV2, VV2)
grouper anchor1 anchor2 points = (leftGroup, rightGroup)
  where
    leftGroup = V.filter (\z -> determinant anchor1 anchor2 z > 0) points
    rightGroup = V.filter (\z -> determinant anchor1 anchor2 z < 0) points

{-Calculates determinant-}
determinant :: V2 -> V2 -> V2 -> Double
determinant anchor1 anchor2 point = (w1 - z1) * (u2 - z2) - (w2 - z2) * (u1 - z1)
  where
    z1 = fst anchor1 
    z2 = snd anchor1 
    w1 = fst anchor2 
    w2 = snd anchor2 
    u1 = fst point 
    u2 = snd point 

{-Area for triangle-}
triArea :: V2 -> V2 -> V2 -> Double
triArea v1 v2 v3 =
    let x1 = fst v1 
        y1 = snd v1 
        x2 = fst v2 
        y2 = snd v2 
        x3 = fst v3 
        y3 = snd v3 
    in abs ((x1 * (y2 - y3)) + (x2 * (y3 - y1)) + (x3 * (y1 - y2)))

