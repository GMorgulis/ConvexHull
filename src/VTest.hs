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
import Qhseq (qh)


-- Generate a random point (x, y) where x and y are between -x and x
vRandomPoint :: Double -> IO V2
vRandomPoint x = do
    xCoord <- randomRIO (-x, x)
    yCoord <- randomRIO (-x, x)
    return $ V.fromList [xCoord, yCoord]

-- Generate a list of random points (n points)
vGeneratePoints :: Int -> IO VV2
vGeneratePoints n = V.fromList <$> sequence (replicate n (vRandomPoint 100000000))


vv2ToListOfTuples :: VV2 -> [(Double, Double)] 
vv2ToListOfTuples vv2 = V.toList $ V.map (\v -> (v V.! 0, v V.! 1)) vv2

{-This test does not check for correctnes. Instead, it should be used for testing time-}
oneTest :: IO ()
oneTest = do 
    print "Starting Point Generation"
    points <- vGeneratePoints 10000000
    
    let convertedPoint = vv2ToListOfTuples points

    print "Starting Test"
    startT <- getCPUTime
    let parPoints = quickh points 8
    endT <- parPoints `deepseq` getCPUTime
    print(endT - startT)

    print "Starting Seq Test"
    startTime <- getCPUTime
    let seqPoints = qh convertedPoint
    endTime <- seqPoints `deepseq` getCPUTime
    print (endTime - startTime)

    print "Complete!"


--stack exec convex-hull-exe -- +RTS -ls -s -N2