module Main (main) where

import qualified Data.Vector.Unboxed as VU
import QuickHullV (VV2, quickh)
import System.CPUTime
import Control.DeepSeq
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import GHC.Conc (getNumCapabilities)
import System.Environment (getArgs)

{-
George Morgulis(gm3138)
Henry Lin(hkl2127)
-}

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
{-
Function to read file and run quickhull. Use deepseq on both processes to help measure CPU time.
Requires input file but does not produce an output file for timing purposes.
-}
main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName] -> do
            threads <- getNumCapabilities 
            putStrLn $ "Running with " ++ show threads
            print "Reading:"
            print fileName
            points <- readPointsFromFile fileName
            print (VU.length points)
            points `deepseq` putStrLn "Points have been fully read and evaluated"
            print "Starting Test"
            startT <- getCPUTime
            let parPoints = quickh points threads
            endT <- parPoints `deepseq` getCPUTime
            print (div (endT - startT) 1000000000)
            print (endT - startT)
            print (VU.length parPoints)
            print "done"
        _ -> putStrLn "Usage: program <input_file>"





 
