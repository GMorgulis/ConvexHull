module VTest
    (oneTest)
    where

import qualified Data.Vector as V
import System.Random (randomRIO)
import QuickHullV (V2, VV2, quickh)
import System.CPUTime
import Data.List(sort, (\\))
import Control.DeepSeq


-- Generate a random point (x, y) where x and y are between -x and x
vRandomPoint :: Double -> IO V2
vRandomPoint x = do
    xCoord <- randomRIO ((-1) * x, x)
    yCoord <- randomRIO ((-1) * x, x)
    return $ V.fromList [xCoord, yCoord]

-- Generate a list of random points (n points)
vGeneratePoints :: Int -> IO VV2
vGeneratePoints n = V.fromList <$> sequence (replicate n (vRandomPoint 400))

{-This test does not check for correctnes. Instead, it should be used for testing time-}
oneTest :: IO ()
oneTest = do 
    print "Starting Point Generation"
    points <- vGeneratePoints 400

    print points

    print "Starting Test"

    startT <- getCPUTime
    let parPoints = quickh points 
    endT <- parPoints `deepseq` getCPUTime
    print (endT - startT)

    print parPoints


    print "Complete!"


