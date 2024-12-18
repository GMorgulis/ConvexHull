module Qhseq
    (C2, qh
    ) where

{-
George Morgulis(gm3138)
Henry Lin(hkl2127)
COMS 4995 Parallel Functional Programming

This is my very first sequential implmentation of Quick Hull.
(!!!) This code is not meant to be run as the project greatly changed since I wrote this first 
implementation. This simply exists to show how far the project has progressed. 
-}

import Data.List (maximumBy, minimumBy, nub)
import Data.Ord (comparing)

{- Type used to represent points-}
type C2 = (Double, Double)

qh :: [C2] -> [C2]
qh points = (helper1 points [])
    where
        helper1 [] hull = hull -- starter 
        helper1 (x : xs) hull =
            let m1 = maxAreaPoint a1 a2 group1
                m2 = maxAreaPoint a1 a2 group2
                group1 = fst (grouper a1 a2 (x:xs))
                group2 = snd (grouper a1 a2 (x:xs))
                a1 = mind (x:xs) 0
                a2 = maxd (x:xs) 0
            in a1 : a2 : helper2 a1 a2 m1 (keepOuter a1 a2 m1 group1) (m1 : hull) ++ helper2 a2 a1 m2 (keepOuter a1 a2 m2 group2) (m2 : hull)

        helper2 _ _ _ [] hull = hull -- calculates lower and upper hull
        helper2 o1 o2 pm (y:ys) hull =
            let m1 = maxAreaPoint o1 pm group1
                m2 = maxAreaPoint o2 pm group2
                group1 = fst (grouper o1 pm (y:ys)) -- always picking the left
                group2 = fst (grouper pm o2 (y:ys)) -- alwyas picking the left
            in helper2 o1 pm m1 (keepOuter o1 pm m1 group1) (m1 : hull) ++ helper2 pm o2 m2 (keepOuter o2 pm m2 group2) (m2 : hull) -- important: note the order of points

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
