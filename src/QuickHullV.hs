module QuickHullV (V2, VV2, quickh) where

{-
George Morgulis 
COMS 4995 Parallel Functional Programming

This is the parallel implmenation of the QuickHull Algorithm to compute the convex hull. The
code be run seqentially by using the -N0, option or in parallel using -NX, where X is the 
amount of threads. Generally, the benefits of paralleism cannot be seen for sets of points that
are smaller than 100,000. The algorithm takes an Unboxed Vector Doubles and returns an Unboxed
Vector of Doubles. The return Vector is the convex set that makes up the hull. Intermediate 
co-linear points are never included in convex set as they are redundant. 
-}


import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as V
import Control.Parallel (par, pseq)

type V2 = (Double, Double)
type VV2 = V.Vector V2

--------------------------------------------------------------------------------------------
{-Returns the convex hull. Find points with the minimum and maxmimum x-coordinates. Adds
  these extremes to hull. Uses the line segment created by the extremes to divide the whole 
  set of points into two groups, and runs ph on both of them.
-}
quickh :: VV2 -> Int -> VV2
quickh points d = V.cons a1 (V.cons a2 hpar)
  where
    a1 = minv points
    a2 = maxv points
    h1 = ph points a1 a2 (depthUpdate d)
    h2 = ph points a2 a1 (depthUpdate d) -- note the change in order
    hpar =  h1 `par` (h2 `pseq` V.concat [h1, h2]) 

{-Helper function for quick hull. Uses the two extemes given as parameter to for a line segment.
  Only points to the left of the line segment are kept; all others are eleminated. Finds "m1" the 
  point that is furthest from the line segment and adds it the hull. Concatenates results of ph with 
  a1, m1 and m1, a2. Runs in parallel if depth is positive, length is greater than baseline and 
  if percent points remaining after the "groupping" is greater than 30% of "points".
-}
ph :: VV2 -> V2 -> V2 -> Int -> VV2
ph points a1 a2 d
  | V.length group == 0 = V.empty 
  | d > 0 && V.length group > 250000 && percentRemaining > 0.3 = V.cons m1 hpar
  | otherwise = V.cons m1 h
  where
    percentRemaining = fromIntegral (V.length group) / fromIntegral (V.length points) :: Double
    group = grouper a1 a2 points
    m1 = maxAreaPoint a1 a2 group
    h1 = ph group a1 m1 (depthUpdate d)
    h2 = ph group m1 a2 (depthUpdate d) -- note the change in order 
    h = V.concat [h1, h2]
    hpar = h1 `par` (h2 `pseq` V.concat [h1, h2]) 

{-Function to find the point with the maximum x-coordinate-}
maxv :: VV2 -> V2
maxv points = V.maximumBy (comparing fst) points

{-Function to find the point with the maximum x-coordinate-}
minv :: VV2 -> V2
minv points = V.minimumBy (comparing fst) points

{-Furthest point from a line segment defined by points a1, a2-}
maxAreaPoint :: V2 -> V2 -> VV2 -> V2
maxAreaPoint anchor1 anchor2 points = V.maximumBy (comparing (triArea anchor1 anchor2)) points

{-Groups by 2D determiants (cross product). Always return the points left of segment (a1,a2)-}
grouper :: V2 -> V2 -> VV2 -> VV2
grouper anchor1 anchor2 points = V.filter (\z -> determinant anchor1 anchor2 z > 0) points

{-Calculates 2d determinant, cross product-}
determinant :: V2 -> V2 -> V2 -> Double
determinant (z1, z2) (w1, w2) (u1, u2) = (w1 - z1) * (u2 - z2) - (w2 - z2) * (u1 - z1)

{-Area for triangle-}
triArea :: V2 -> V2 -> V2 -> Double
triArea (z1, z2) (w1, w2) (u1, u2) = abs ((z1 * (w2 - u2)) + (w1 * (u2 - z2)) + (u1 * (z2 - w2)))

{-Updates the depth of recursion-}
depthUpdate :: Int -> Int
depthUpdate d
  | d <= 1 = 0
  | otherwise = div d 2