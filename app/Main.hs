module Main (main) where

import Data.List(sort, (\\))
import Qhseq (qh)
import Diagrams.Prelude
import Diagrams.Backend.SVG (renderSVG)
import Andrew (convexHull)
import System.Exit (exitSuccess, exitFailure)
import Draw (generateRandomPoints, diagram)


main :: IO ()
main = do
    points <- generateRandomPoints 300
    let hullPoints = sort (qh points)
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
    print "All points:"
    print poops
    print "----------------------------------------------------"

    if hullPoints == correctPoints 
        then do 
            print "Sucess"
            exitFailure
        else do 
            print "Failure"
            exitSuccess







poops = [(137, 296), (12, 288), (236, 119), (51, 130), (276, 165), (97, 298), (320, 188), (264, 303), (349, 314), (62, 99),
    (120, 139), (135, 50), (196, 106), (292, 287), (101, 131), (28, 80), (121, 100), (272, 241), (231, 351), (387, 170),
    (99, 190), (282, 318), (119, 233), (244, 192), (191, 92), (147, 13), (228, 66), (359, 94), (35, 196), (178, 243),
    (306, 216), (110, 96), (293, 372), (46, 369), (44, 305), (197, 238), (226, 177), (56, 9), (344, 41), (145, 34),
    (141, 12), (332, 222), (191, 289), (177, 53), (5, 161), (54, 283), (116, 119), (72, 168), (181, 113), (33, 94),
    (39, 292), (111, 328), (315, 85), (48, 226), (195, 74), (271, 159), (310, 352), (369, 348), (133, 228), (178, 70),
    (176, 224), (16, 13), (275, 45), (98, 222), (344, 53), (314, 374), (141, 121), (72, 100), (194, 220), (87, 6),
    (132, 12), (146, 195), (314, 253), (45, 39), (128, 166), (354, 48), (365, 249), (60, 67), (227, 271), (94, 55),
    (40, 148), (229, 327), (146, 348), (221, 69), (47, 345), (225, 330), (303, 220), (60, 82), (353, 122), (259, 48),
    (340, 140), (83, 193), (370, 25), (350, 78), (145, 182), (53, 57), (261, 245), (354, 240), (341, 211), (271, 64),
    (9, 84), (272, 47), (18, 310), (89, 213), (131, 246), (56, 30), (38, 70), (30, 110), (100, 285), (191, 38),
    (64, 159), (23, 100), (219, 292), (75, 70), (193, 176), (206, 27), (187, 267), (66, 312), (155, 151), (246, 194)]