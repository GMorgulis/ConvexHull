{-
George Morgulis 
COMS 4995 Parallel Functional Programming

This is the sequential implementaion of quickhull
-}

import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)


type C2 = (Double, Double)


{-
parthull :: C2 -> C2 -> [C2] -> [C2]
parthull anchor1 anchor2 (x:xs) = helper anchor1 anchor2 (x:xs) []
    where 
        helper-}

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
maxArea :: C2 -> C2 -> [C2] -> C2
maxArea anchor1 anchor2 = maximumBy (comparing (triArea anchor1 anchor2))

{-Determines whether a point lies to the right or left of a line segment-}
grouper :: C2 -> C2 -> [C2] -> ([C2],[C2])
grouper anchor1 anchor2 points = helper anchor1 anchor2 points [] []
    where
        helper _ _ [] group1 group2 = (group1, group2)
        helper (x1, y1) (x2, y2) (z:zs) group1 group2
            |  (x2 - x1) * (snd z - y1) - (y2 - y1) * (fst z - x1) > 0 = helper (x1, y1) (x2, y2) zs (z : group1) group2
            | otherwise = helper (x1, y1) (x2, y2) zs group1 (z : group2)


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
triArea (x1, y1) (x2, y2) (x3, y3) = (0.5) * abs ((x1 * (y2 - y3)) + (x2 * (y3 - y1)) + (x3 * (y1 - y2)))

{-Checks if point is in triangle-}
pointInTriangle :: C2 -> C2 -> C2 -> C2 -> Bool
pointInTriangle t1 t2 t3 p = (triArea t1 t2 p) + (triArea t1 t3 p) + (triArea t2 t3 p) == (triArea t1 t2 t3)

