module Main (main) where

    
import Qhseq (quickHull)

main :: IO ()
main =
    print (result)
    where
        result = quickHull [(x, y) | x <- [0..9], y <- [0..9]]
        --expected = [(0, 0), (9, 0), (9, 9), (0, 9)]
