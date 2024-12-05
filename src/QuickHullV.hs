module QuickHullV (V2, VV2, quickh, maxAreaPoint, triArea, grouper, maxv, minv, keepOuter)where

{-
George Morgulis 
COMS 4995 Parallel Functional Programming

This is the sequential implementaion of quickhull
-}


import Data.Ord (comparing)
import qualified Data.Vector as V
import Control.Parallel (par, pseq)
import Control.DeepSeq
--Control.Parallel.Strategies (parList, rseq, using)


{-New vector type-}
type V2 = V.Vector Double
type VV2 = V.Vector V2
type VV3 = V.Vector VV2

--------------------------------------------------------------------------------------------

quickh :: VV2 -> VV2
quickh points = V.cons a1 (V.cons a2 h)
  where
    a1 = minv points
    a2 = maxv points
    group1 = (grouper a1 a2 points) V.!0
    group2 = (grouper a1 a2 points) V.!1
    m1 = maxAreaPoint a1 a2 group1
    m2 = maxAreaPoint a1 a2 group2
    hull1 = parHelper a1 a2 m1 (keepOuter a1 a2 m1 group1) (V.singleton m1)
    hull2 = parHelper a2 a1 m2 (keepOuter a2 a1 m2 group2) (V.singleton m2)
    h = hull1 `par` (hull2 `pseq` (hull2 V.++ hull1))


parHelper :: V2 -> V2 -> V2 -> VV2 -> VV2 -> VV2
parHelper _ _ _ points hull | V.null points = hull
parHelper o1 o2 pm points hull 
  | length points > 100000 = p
  | otherwise = shull1 V.++ shull2 
  where
    m1 = maxAreaPoint o1 pm group1
    m2 = maxAreaPoint o2 pm group2
    group1 = (grouper o1 pm points) V.!0
    group2 = (grouper pm o2 points) V.!0
    p = phull1 `par` (phull2 `pseq` (phull2 V.++ phull1))
    phull1 = parHelper o1 pm m1 (keepOuter o1 pm m1 group1) (V.cons m1 hull)
    phull2 = parHelper pm o2 m2 (keepOuter o2 pm m2 group2) (V.cons m2 hull)
    shull1 = seqHelper o1 pm m1 (keepOuter o1 pm m1 group1) (V.cons m1 hull)
    shull2 = seqHelper pm o2 m2 (keepOuter o2 pm m2 group2) (V.cons m2 hull)


seqHelper :: V2 -> V2 -> V2 -> VV2 -> VV2 -> VV2
seqHelper _ _ _ points hull | V.null points = hull
seqHelper o1 o2 pm points hull = hull2 V.++ hull1
  where
    m1 = maxAreaPoint o1 pm group1
    m2 = maxAreaPoint o2 pm group2
    group1 = (grouper o1 pm points) V.!0
    group2 = (grouper pm o2 points) V.!0
    hull1 = seqHelper o1 pm m1 (keepOuter o1 pm m1 group1) (V.cons m1 hull)
    hull2 = seqHelper pm o2 m2 (keepOuter o2 pm m2 group2) (V.cons m2 hull)


{-Function to find the V2 with the maximum x-coordinate-}
maxv :: VV2 -> V2
maxv = V.maximumBy (comparing (V.! 0))

{-Function to find the V2 with the maximum x-coordinate-}
minv :: VV2 -> V2
minv = V.minimumBy (comparing (V.! 0))

{-Furthest point from a line-}
maxAreaPoint :: V2 -> V2 -> VV2 -> V2
maxAreaPoint _ anchor2 points | V.null points = anchor2
maxAreaPoint anchor1 anchor2 points = V.maximumBy (comparing (triArea anchor1 anchor2)) points

{-Groups by determinant (left, right)-}
grouper :: V2 -> V2 -> VV2 -> VV3
grouper anchor1 anchor2 points = V.fromList [leftGroup, rightGroup]
  where
    leftGroup = V.filter (\z -> determinant anchor1 anchor2 z > 0) points
    rightGroup = V.filter (\z -> determinant anchor1 anchor2 z < 0) points

{-Calculates determinant-}
determinant :: V2 -> V2 -> V2 -> Double
determinant anchor1 anchor2 point = (w1 - z1) * (u2 - z2) - (w2 - z2) * (u1 - z1)
  where
    z1 = anchor1 V.! 0
    z2 = anchor1 V.! 1
    w1 = anchor2 V.! 0
    w2 = anchor2 V.! 1
    u1 = point V.! 0
    u2 = point V.! 1

{-Keeps points outside a triangle-}
keepOuter :: V2 -> V2 -> V2 -> VV2 -> VV2
keepOuter t1 t2 t3 = V.filter (not . pointInTriangle t1 t2 t3)

{-Finds points inside a triangle-}
pointInTriangle :: V2 -> V2 -> V2 -> V2 -> Bool
pointInTriangle t1 t2 t3 p =
    closeEnough (triArea t1 t2 p + triArea t1 t3 p + triArea t2 t3 p) (triArea t1 t2 t3)

{-Area for triangle-}
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