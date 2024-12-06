module Main (main) where


import System.Random (randomRIO)
import qualified Data.Vector as V
import Control.Monad (replicateM)
import System.IO (readFile)
import QuickHullV (V2, VV2, quickh)
import System.CPUTime
import Data.List(sort, (\\))
import Control.DeepSeq
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC 


-- Function to parse a ByteString line into a tuple of doubles
parseLine :: B.ByteString -> (Double, Double)
parseLine line =
    let [x, y] = map (read . BC.unpack) (BC.split ',' line)
    in (x, y)

-- Function to convert a list of tuples to VV2
listToVV2 :: [(Double, Double)] -> VV2
listToVV2 = V.fromList . map (\(x, y) -> V.fromList [x, y])

-- Function to read and parse the file into a VV2
readPointsFromFile :: FilePath -> IO VV2
readPointsFromFile filePath = do
    content <- B.readFile(filePath)
    let linesOfFile = BC.lines content
        pointsList = map parseLine linesOfFile
    return $ listToVV2 pointsList



main :: IO ()
main = do
    print "here"
    runner
    --generator


runner :: IO ()
runner = do
    print "Reading"
    points <- readPointsFromFile "random_points.txt"
    --print points
    print "Starting Test"
    startT <- getCPUTime
    let parPoints = quickh points 8
    endT <- parPoints `deepseq` getCPUTime
    print(endT - startT)
    print "done"



-- Function to generate a random point within a specified range
vRandomPoint :: Double -> IO V2
vRandomPoint range = do
    x <- randomRIO (-range, range)
    y <- randomRIO (-range, range)
    return $ V.fromList [x, y]

-- Generate a list of random points (n points)
vGeneratePoints :: Int -> IO VV2
vGeneratePoints n = V.fromList <$> replicateM n (vRandomPoint 100000000)

-- Convert VV2 to a list of tuples
vv2ToListOfTuples :: VV2 -> [(Double, Double)]
vv2ToListOfTuples vv2 = V.toList $ V.map (\v -> (v V.! 0, v V.! 1)) vv2

-- Function to generate points and write to a file
generatePointsAndWriteToFile :: Int -> FilePath -> IO ()
generatePointsAndWriteToFile n filePath = do
    points <- vGeneratePoints n
    let pointsList = vv2ToListOfTuples points
    writeFile filePath $ unlines $ map (\(x, y) -> show x ++ "," ++ show y) pointsList

generator :: IO ()
generator = generatePointsAndWriteToFile 10000000 "random_points.txt"

--stack exec convex-hull-exe -- +RTS -ls -s -N2


