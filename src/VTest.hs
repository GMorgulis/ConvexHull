module VTest
    (oneTest)
    where

import qualified Data.Vector as V
import System.Random (randomRIO)
import QuickHullV (V2, VV2, quickh)
import System.CPUTime
import Data.List(sort, (\\))
import Control.DeepSeq
import Andrew (convexHull)
import Data.List (maximumBy, minimumBy, nub)


-- Generate a random point (x, y) where x and y are between -x and x
vRandomPoint :: Double -> IO V2
vRandomPoint x = do
    xCoord <- randomRIO (-x, x)
    yCoord <- randomRIO (-x, x)
    return $ V.fromList [xCoord, yCoord]

-- Generate a list of random points (n points)
vGeneratePoints :: Int -> IO VV2
vGeneratePoints n = V.fromList <$> sequence (replicate n (vRandomPoint 10000000))


vv2ToListOfTuples :: VV2 -> [(Double, Double)] 
vv2ToListOfTuples vv2 = V.toList $ V.map (\v -> (v V.! 0, v V.! 1)) vv2

{-This test does not check for correctnes. Instead, it should be used for testing time-}
oneTest :: IO ()
oneTest = do 
    print "Starting Point Generation"
    points <- vGeneratePoints 100000
    let convertedPoint = vv2ToListOfTuples points

    --print points

    print "Starting Test"

    startT <- getCPUTime
    let parPoints = quickh points 
    endT <- parPoints `deepseq` getCPUTime
    --print (endT - startT)

    let correctPoints = sort (convexHull convertedPoint)

    print ("cooorect")
    print correctPoints

    print "my "
    let myPoints = (sort (nub (vv2ToListOfTuples parPoints)))
    print myPoints

    print (correctPoints == myPoints)




    print "Complete!"


