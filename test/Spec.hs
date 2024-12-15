import qualified Data.Vector.Unboxed as VU
import System.Random (randomRIO)
import QuickHullV (V2, VV2, quickh)
import Data.List (sort, nub)
import Andrew (convexHull)
import Qhseq (qh)



-- Generate a random point (x, y) where x and y are between -x and x
vRandomPoint :: Double -> IO V2
vRandomPoint x = do
    xCoord <- randomRIO (-x, x)
    yCoord <- randomRIO (-x, x)
    return (xCoord, yCoord)

-- Generate a list of random points (n points)
vGeneratePoints :: Int -> IO VV2
vGeneratePoints n = VU.replicateM n (vRandomPoint 100000000)

-- Convert VV2 to a list of tuples
vv2ToListOfTuples :: VV2 -> [(Double, Double)]
vv2ToListOfTuples = VU.toList

{-This test does not check for correctness. Instead, it should be used for testing time-}
main :: IO ()
main = do
    print "Starting Point Generation"
    points <- vGeneratePoints 1000

    let convertedPoint = vv2ToListOfTuples points
    let seqPoints = sort (nub (qh convertedPoint))
    let parPoints = vv2ToListOfTuples (quickh points 8)
    let andrewPoints = sort (nub (convexHull convertedPoint))

    print (length parPoints)
    print (length seqPoints)
    print (length andrewPoints)

    if (sort(nub parPoints) == sort(nub(seqPoints)))
        then print "Test Passed!"
        else print "Test Failed!"
    
    if (sort(nub parPoints) == sort(nub(andrewPoints)))
        then print "Andrew Passed"
        else print "Andew Failed"

    print "Complete!"

--stack exec convex-hull-exe -- +RTS -ls -s -N2
