{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Test 
    (
        singleTest,
        multiTest,
        timeTest 
    ) where 

import Data.List(sort, (\\))
import Qhseq (qh)
import Diagrams.Prelude
import Diagrams.Backend.SVG (renderSVG)
import Andrew (convexHull)
import System.Exit (exitSuccess, exitFailure)
import Draw (generateRandomPoints, diagram)
import Qhpar (qhull)
import System.CPUTime
import Control.DeepSeq

{-This generates a single random sample set of points and runs the quickHull algorithm.
The data is checked against the correct implementation of Andrew's Algorithm (Monotone Chain".
An svg image of the hull is created.-}
singleTest :: IO ()
singleTest = do 
    points <- generateRandomPoints 400
    let hullPoints = sort (qhull points)
    let correctPoints = sort (convexHull points)
    renderSVG "convexHull_with_axes.svg" (mkWidth 500) (diagram points hullPoints)
    print "----------------------------------------------------"


    if hullPoints == correctPoints 
        then do
            print "Test Passed"
       else do
            putStrLn "They do not match"
            putStrLn $ "Extra Points: " ++ show (hullPoints \\ correctPoints)
            putStrLn $ "Missing Points: " ++ show (correctPoints \\ hullPoints)

    print "----------------------------------------------------"
    print "My Implementation:"
    print hullPoints
    print "Length"
    print $ length hullPoints
    print "----------------------------------------------------"
    print "Correct Implementation"
    print correctPoints
    print "Correct Length"
    print $ length correctPoints
    print "----------------------------------------------------"
    --print "All points:"
    --print points
    --print "----------------------------------------------------"

    if hullPoints == correctPoints 
        then do 
            print "Sucess"
            exitSuccess
        else do 
            print "Failure"
            exitFailure

{-This option is for testing high volume of random samples that works in relationship with the bash script
called "tester.sh". This option does not give any feedback on the data, just brute force testing.-}
multiTest :: IO ()
multiTest = do
    points <- generateRandomPoints 1000000
    let hullPoints = sort (qh points)
    let correctPoints = sort (convexHull points)
    print "----------------------------------------------------"

    if hullPoints == correctPoints 
        then do 
            print "Sucess"
            exitSuccess
        else do 
            print "Failure"
            exitFailure

{-This test does not check for correctnes. Instead, it should be used for testing time-}
timeTest :: IO ()
timeTest = do 
    points <- generateRandomPoints 10000000

    let correctPoints = sort (convexHull points)

    startTime <- getCPUTime
    let seqPoints = sort (qh points)
    endTime <- seqPoints `deepseq` getCPUTime
    print (endTime - startTime)


    startTime <- getCPUTime
    let parPoints = sort (qhull points)
    endTime <- parPoints `deepseq` getCPUTime
    print (endTime - startTime)

    if seqPoints == parPoints
        then  
            print "good"
        else 
            print "bad"

    if seqPoints == correctPoints 
        then 
            print "seq works"
        else 
            print "seq not working"

    if parPoints == correctPoints 
        then 
            print "par works"
        else 
            print "par not workin"

    print "Complete!"



-- 50000000 for time testing (3m 24)
-- 10000000