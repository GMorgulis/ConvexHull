module Main (main) where


import Qhseq (mind, maxd, qHull, C2 )
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG (renderSVG)
import System.IO (writeFile)

-- Define your points
points :: [(Double, Double)]
points = [(90.5, 120.3), (210.8, 310.4), (150.1, 50.9), (400.7, 300.2), (320.4, 180.6),
 (250.2, 120.8), (370.1, 450.3), (80.3, 200.5), (600.2, 350.1), (140.9, 60.7),
 (310.5, 400.2), (450.8, 270.4), (110.7, 50.2), (500.1, 250.3), (180.9, 90.5),
 (250.4, 320.7), (370.6, 180.3), (130.2, 150.6), (420.1, 200.8), (240.3, 70.1)]

-- Convert points to P2 type used by Diagrams
pointsP2 :: [P2 Double]
pointsP2 = map p2 points

-- Get the convex hull
hullPoints :: [(Double, Double)]
hullPoints = qHull points

-- Convert hull points to P2 type
hullPointsP2 :: [P2 Double]
hullPointsP2 = map p2 hullPoints

-- Draw the points with hull points in red and the rest in blue
pointsDiagram :: Diagram B
pointsDiagram = mconcat
    [ mconcat [ circle 5 # fc blue # translate (r2 (x, y))
              | (x, y) <- points, (x, y) `notElem` hullPoints ]
    , mconcat [ circle 5 # fc red # translate (r2 (x, y))
              | (x, y) <- hullPoints ]
    ]

-- Draw x and y axes
axes :: Diagram B
axes = (arrowBetween (p2 (0, -50)) (p2 (0, 400)) # lc black) <>
       (arrowBetween (p2 (-50, 0)) (p2 (400, 0)) # lc black)

-- Combine points diagram with axes
diagram :: Diagram B
diagram = pointsDiagram <> axes

main :: IO ()
main = do
    renderSVG "convexHull_with_axes.svg" (mkWidth 300) diagram
    print (hullPoints)
    print (length (hullPoints))
    print (maxd hullPoints 0)
    print (mind hullPoints 0)
