module Qhseq 
    (C2, mind, maxd, qh
    ) where
      
{-
George Morgulis 
COMS 4995 Parallel Functional Programming

This is the sequential implementaion of quickhull
-}

import Data.List (maximumBy, minimumBy, nub)
import Data.Ord (comparing)


type C2 = (Double, Double)


qh :: [C2] -> [C2]
qh points = nub (helper1 points [])
    where 
        helper1 [] hull = hull -- starter 
        helper1 (x : xs) hull =
            let m1 = maxAreaPoint a1 a2 group1
                m2 = maxAreaPoint a1 a2 group2
                group1 = fst (grouper a1 a2 (x:xs))
                group2 = snd (grouper a1 a2 (x:xs))
                a1 = mind (x:xs) 0 
                a2 = maxd (x:xs) 0 
            in a1 : a2 : helper2 a1 a2 m1 (keepOuter a1 a2 m1 group1) (m1 : hull) ++ helper3 a1 a2 m2 (keepOuter a1 a2 m2 group2) (m2 : hull)

        helper2 _ _ _ [] hull = hull -- upper hull
        helper2 o1 o2 pm (y:ys) hull =
            let m1 = maxAreaPoint o1 pm group1
                m2 = maxAreaPoint o2 pm group2
                group1 = fst (grouper o1 pm (y:ys)) -- always picking the elet
                group2 = fst (grouper pm o2 (y:ys)) -- alwyas picking the left
            in helper2 o1 pm m1 (keepOuter o1 pm m1 group1) (m1 : hull) ++ helper2 o2 pm m2 (keepOuter o2 pm m2 group2) (m2 : hull)
        
        helper3 _ _ _ [] hull = hull -- lower hull
        helper3 o1 o2 pm (y:ys) hull =
            let m1 = maxAreaPoint o1 pm group1
                m2 = maxAreaPoint o2 pm group2
                group1 = snd (grouper o1 pm (y:ys))  -- always picking the right
                group2 = snd (grouper pm o2 (y:ys))  -- always picking the right
            in helper3 o1 pm m1 (keepOuter o1 pm m1 group1) (m1 : hull) ++ helper3 o2 pm m2 (keepOuter o2 pm m2 group2) (m2 : hull)


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
maxAreaPoint anchor1 anchor2 [] = anchor2
maxAreaPoint anchor1 anchor2 points = maximumBy (comparing (triArea anchor1 anchor2)) points

{-Determines whether a point lies to the right or left of a line segment-}
grouper :: C2 -> C2 -> [C2] -> ([C2],[C2])
grouper anchor1 anchor2 points = helper anchor1 anchor2 points [] []
    where
        helper _ _ [] group1 group2 = (group1, group2)
        helper (x1, y1) (x2, y2) (z:zs) group1 group2
            | (x1 == fst z && y1 == snd z) || (x2 == fst z && y2 == snd z) = helper (x1, y1) (x2, y2) zs group1 group2  -- Makes sure anchors are not added
            | (x2 - x1) * (snd z - y1) - (y2 - y1) * (fst z - x1) == 0 = helper (x1, y1) (x2, y2) zs group1 group2 
            | (x2 - x1) * (snd z - y1) - (y2 - y1) * (fst z - x1) > 0 = helper (x1, y1) (x2, y2) zs (z : group1) group2 -- Uses cross product, adds g1
            | otherwise = helper (x1, y1) (x2, y2) zs group1 (z : group2) -- add to g2


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
    closeEnough (abs (triArea t1 t2 p) + abs (triArea t1 t3 p) + abs (triArea t2 t3 p)) (abs (triArea t1 t2 t3))

epsilon :: Double
epsilon = 1e-9

closeEnough :: Double -> Double -> Bool
closeEnough a b = abs (a - b) < epsilon