module Draw 
    (diagram, generateRandomPoints
    ) where

import Data.List(sort, (\\))
import Qhseq (mind, maxd, C2, qh)
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG (renderSVG)
import System.Random (randomRIO)
import Andrew (convexHull)
import System.Exit (exitSuccess, exitFailure)

-- Generate a random point (x, y) where x and y are between 0.0 and 400.0
randomPoint :: Double -> IO (Double, Double)
randomPoint x = do
    x <- randomRIO ((-1 * x), x)
    y <- randomRIO ((-1 * x), x)
    return (x, y)

-- Generate a list of random points (n points)
generateRandomPoints :: Int -> IO [(Double, Double)]
generateRandomPoints n = sequence $ replicate n (randomPoint 100000000)

-- Draw the points with hull points in red and the rest in blue
pointsDiagram :: [(Double, Double)] -> [(Double, Double)] -> Diagram B
pointsDiagram points hullPoints = mconcat
    [ mconcat [ circle 2 # fc blue # lcA transparent # translate (r2 (x, y))
              | (x, y) <- points, (x, y) `notElem` hullPoints ]
    , mconcat [ circle 2 # fc red # lcA transparent # translate (r2 (x, y))
              | (x, y) <- hullPoints ]
    ]

-- Draw x and y axes
axes :: Diagram B
axes = (arrowBetween (p2 (0, -50)) (p2 (0, 410)) # lc black) <>
       (arrowBetween (p2 (-50, 0)) (p2 (410, 0)) # lc black)

-- Combine points diagram with axes
diagram :: [(Double, Double)] -> [(Double, Double)] -> Diagram B
diagram points hullPoints = pointsDiagram points hullPoints <> axes