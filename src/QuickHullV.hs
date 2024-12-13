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
maxv = V.maximumBy (comparing fst)

{-Function to find the V2 with the maximum x-coordinate-}
minv :: VV2 -> V2
minv = V.minimumBy (comparing fst)

{-Furthest point from a line-}
maxAreaPoint :: V2 -> V2 -> VV2 -> V2
maxAreaPoint _ _ points | V.null points = error "missing points"
maxAreaPoint anchor1 anchor2 points = V.maximumBy (comparing (triArea anchor1 anchor2)) points


{-
maxAreaPoint :: V2 -> V2 -> VV2 -> V2
maxAreaPoint _ anchor2 points | V.null points = anchor2
maxAreaPoint anchor1 anchor2 points = V.maximumBy (comparing (triArea anchor1 anchor2)) points  -}

{-Groups by determinant (left, right)-}
grouper :: V2 -> V2 -> VV2 -> VV2
grouper anchor1 anchor2 points = leftGroup
  where
    leftGroup = V.filter (\z -> determinant anchor1 anchor2 z > 0) points
    --rightGroup = V.filter (\z -> determinant anchor1 anchor2 z < 0) points


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

depthUpdate :: Int -> Int
depthUpdate d
  | d <= 1 = 0
  | otherwise = div d 2