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
maxArea anchor1 anchor2 points = maximumBy (comparing(\p -> triArea anchor1 anchor2 p)) points

{-Determines whether a point lies to the right or left of a line segment-}
-- TO-DO

{-Cross Product-}
-- TO-DO

{-Finds the area of a triangle-}
triArea :: C2 -> C2 -> C2 -> Double 
triArea (x1, y1) (x2, y2) (x3, y3) = (0.5) * abs ((x1 * (y2 - y3)) + (x2 * (y3 - y1)) + (x3 * (y1 - y2)))

{-Checks if point is in triangle-}
pointInTriangle :: C2 -> C2 -> C2 -> C2 -> Bool
pointInTriangle t1 t2 t3 p = (triArea t1 t2 p) + (triArea t1 t3 p) + (triArea t2 t3 p) == (triArea t1 t2 t3)
