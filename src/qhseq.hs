{-
George Morgulis 
COMS 4995 Parallel Functional Programming

This is the sequential implementaion of quickhull
-}

import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)

quickhull :: IO ()
quickhull = do
    print ("Hello World")


type C2 = (Double, Double)

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
maxArea anchor1 anchor2 points = maximumBy (comparing(\p -> triarea anchor1 anchor2 p)) points

{-Finds the area of a triangle-}
triarea :: C2 -> C2 -> C2 -> Double 
triarea (x1, y1) (x2, y2) (x3, y3) = (0.5) * abs ((x1 * (y2 - y3)) + (x2 * (y3 - y1)) + (x3 * (y1 - y2)))