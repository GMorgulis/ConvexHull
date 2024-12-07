import qualified Data.Vector.Unboxed as VU
import System.Random (randomRIO)
import QuickHullV (V2, VV2, quickh, minv)
import System.CPUTime
import Data.List (sort, (\\), maximumBy, minimumBy, nub)
import Control.DeepSeq
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
    points <- vGeneratePoints 10000000

    let convertedPoint = vv2ToListOfTuples points

    print (minv points)

    print "Starting Seq Test"
    startTime <- getCPUTime
    let seqPoints = sort (qh convertedPoint)
    endTime <- seqPoints `deepseq` getCPUTime
    print (endTime - startTime)

    print "Starting Test"
    startT <- getCPUTime
    let parPoints = sort (nub (vv2ToListOfTuples (quickh points 8)))
    endT <- parPoints `deepseq` getCPUTime
    print (endT - startT)

    let andewPoints = sort (nub (convexHull convertedPoint))

    print (length parPoints)
    print (length seqPoints)
    if seqPoints == parPoints
        then print "Test Passed!"
        else print "Test Failed!"
    
    if seqPoints == andewPoints 
        then print "Andrew Passed"
        else print "Andew Failed"
    

    print "Complete!"

--stack exec convex-hull-exe -- +RTS -ls -s -N2
