module Main (main) where

import qualified Data.Vector.Unboxed as VU
import QuickHullV (V2, VV2, quickh, maxv, minv, maxAreaPoint, grouper)
import System.CPUTime
import Data.List (sort, (\\))
import Control.DeepSeq
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import GHC.Conc (getNumCapabilities)

-- Function to parse a ByteString line into a tuple of doubles
parseLine :: B.ByteString -> (Double, Double)
parseLine line =
    let [x, y] = map (read . BC.unpack) (BC.split ',' line)
    in (x, y)

-- Function to convert a list of tuples to VV2
listToVV2 :: [(Double, Double)] -> VV2
listToVV2 = VU.fromList

-- Function to read and parse the file into a VV2
readPointsFromFile :: FilePath -> IO VV2
readPointsFromFile filePath = do
    content <- B.readFile(filePath)
    let linesOfFile = BC.lines content
        pointsList = map parseLine linesOfFile
    return $ listToVV2 pointsList

main :: IO ()
main = do
    threads <- getNumCapabilities 
    putStrLn $ "Running with " ++ show threads
    runner threads
    --methodTest

runner :: Int -> IO ()
runner threads = do
    print "Reading"
    points <- readPointsFromFile "random_points1m.txt"
    points `deepseq` putStrLn "Points have been fully read and evaluated"
    --print (maxv points)
    --print points
    print "Starting Test"
    startT <- getCPUTime
    let parPoints = quickh points threads
    endT <- parPoints `deepseq` getCPUTime
    print (div (endT - startT) 1000000000000)
    print (VU.length parPoints)
    print "done"

methodTest :: IO ()
methodTest = do 
    print "Reading"
    points <- readPointsFromFile "random_points.txt"
    print "done reading"
    let a1 = minv points 
    print a1
    let a2 = maxv points
    print a2
    let m1 = maxAreaPoint a1 a2 points
    print m1 
 

    let (g1, g2) = grouper a1 a2 points

    print (maxv g1)
    print (minv g2)
