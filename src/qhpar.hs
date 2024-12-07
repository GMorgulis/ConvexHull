module Qhpar
    (qhull, maxd
    ) where

{-
George Morgulis 
COMS 4995 Parallel Functional Programming

This is my first, really bad parallel implementaion of quickhull
-}

import Data.List (maximumBy, minimumBy, nub)
import Data.Ord (comparing)
import Control.Parallel (par, pseq)


{- Type used to represent points-}
type C2 = (Double, Double)

qhull :: [C2] -> [C2]
qhull points = nub (a1 : a2 : h)
    where
        a1 = mind points 0
        a2 = maxd points 0
        group1 = fst (grouper a1 a2 points)
        group2 = snd (grouper a1 a2 points)
        m1 = maxAreaPoint a1 a2 group1
        m2 = maxAreaPoint a1 a2 group2
        hull1 = parHelper a1 a2 m1 (keepOuter a1 a2 m1 group1) [m1]
        hull2 = parHelper a2 a1 m2 (keepOuter a1 a2 m2 group2) [m2]
        h = par hull1 (hull2 ++ hull1)

{-Parallel verison of hull helper-}
parHelper :: C2 -> C2 -> C2 -> [C2] -> [C2] -> [C2]
parHelper _ _ _ [] hull = hull -- calculates lower and upper hull
parHelper o1 o2 pm (y:ys) hull 
    | length group1 + length group2 > 100000 = par phull1 (phull2 ++ phull1) 
    | otherwise = shull1 ++ shull2
    where 
        m1 = maxAreaPoint o1 pm group1
        m2 = maxAreaPoint o2 pm group2
        group1 = fst (grouper o1 pm (y:ys)) -- always picking the left
        group2 = fst (grouper pm o2 (y:ys)) -- always picking the left
        phull1 = parHelper o1 pm m1 (keepOuter o1 pm m1 group1) (m1 : hull) 
        phull2 = parHelper pm o2 m2 (keepOuter o2 pm m2 group2) (m2 : hull) 
        shull1 = seqHelper o1 pm m1 (keepOuter o1 pm m1 group1) (m1 : hull) 
        shull2 = seqHelper pm o2 m2 (keepOuter o2 pm m2 group2) (m2 : hull) 
 
{-Sequential version of hull helper-}
seqHelper :: C2 -> C2 -> C2 -> [C2] -> [C2] -> [C2]
seqHelper _ _ _ [] hull = hull -- calculates lower and upper hull
seqHelper o1 o2 pm (y:ys) hull = hull2 ++ hull1
    where 
        m1 = maxAreaPoint o1 pm group1
        m2 = maxAreaPoint o2 pm group2
        group1 = fst (grouper o1 pm (y:ys)) -- always picking the left
        group2 = fst (grouper pm o2 (y:ys)) -- always picking the left
        hull1 = seqHelper o1 pm m1 (keepOuter o1 pm m1 group1) (m1 : hull) 
        hull2 = seqHelper pm o2 m2 (keepOuter o2 pm m2 group2) (m2 : hull) 


{-Computes the maximum of points by specified dimension-}
maxd :: [C2] -> Int -> C2
maxd points 0 = maximumBy (comparing fst) points
maxd points 1 = maximumBy (comparing snd) points
maxd _ _ = error "Invalid arguements."

{-Computes the minimum of points by specified dimension-}
mind :: [C2] -> Int -> C2
mind points 0 = minimumBy (comparing fst) points
mind points 1 = minimumBy (comparing snd) points
mind _ _ = error "Invalid arguements."

{-Calculates the maximum area given a line segment-}
maxAreaPoint :: C2 -> C2 -> [C2] -> C2
maxAreaPoint _ anchor2 [] = anchor2
maxAreaPoint anchor1 anchor2 points = maximumBy (comparing (triArea anchor1 anchor2)) points


{-Determines whether a point lies to the right or left of a vector. The first memeber of the return
tuple are the points to the left of the vector, the second are those to the right-}
grouper :: C2 -> C2 -> [C2] -> ([C2],[C2])
grouper anchor1 anchor2 points = helper anchor1 anchor2 points [] []
    where
        helper _ _ [] group1 group2 = (group1, group2)
        helper (x1, y1) (x2, y2) (z:zs) group1 group2
            | closeEnough ((x2 - x1) * (snd z - y1) - (y2 - y1) * (fst z - x1)) 0 = helper (x1, y1) (x2, y2) zs group1 group2 -- collinear points added to neither group
            | (x2 - x1) * (snd z - y1) - (y2 - y1) * (fst z - x1) > 0 = helper (x1, y1) (x2, y2) zs (z : group1) group2 -- left of 
            | otherwise = helper (x1, y1) (x2, y2) zs group1 (z : group2) -- right of 
        


{-Keeps all points outside the triangle. Works for everything other than points on triangle itself-}
keepOuter :: C2 -> C2 -> C2 -> [C2] -> [C2]
keepOuter t1 t2 t3 points = helper t1 t2 t3 points []
    where
        helper _ _ _ [] keep = keep
        helper p1 p2 p3 (x : xs) keep
            | pointInTriangle p1 p2 p3 x = helper p1 p2 p3 xs keep
            | otherwise = helper p1 p2 p3 xs (x : keep)

{-Finds the area of a triangle-}
triArea :: C2 -> C2 -> C2 -> Double
triArea (x1, y1) (x2, y2) (x3, y3) =  abs ((x1 * (y2 - y3)) + (x2 * (y3 - y1)) + (x3 * (y1 - y2)))

{-Checks if point lies inside triangle-}
pointInTriangle :: C2 -> C2 -> C2 -> C2 -> Bool
pointInTriangle t1 t2 t3 p =
    closeEnough (triArea t1 t2 p + triArea t1 t3 p + triArea t2 t3 p) (abs (triArea t1 t2 t3))

{-Epsilon value to mitigate floating point error-}
epsilon :: Double
epsilon = 1e-9

{-Method to mitigate floating point error-}
closeEnough :: Double -> Double -> Bool
closeEnough a b = abs (a - b) < epsilon