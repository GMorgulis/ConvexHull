import qualified QuickHullV as Q
import qualified Data.Vector as V
import QuickHullV (maxAreaPoint, triArea, grouper, maxv, minv, keepOuter)
import Diagrams (tri)



main :: IO ()
main = do
    maxArea 
    area
    group
    maxer
    outer


maxArea :: IO()
maxArea = do
    print "max area test"
    let a1 = V.fromList [0,0] :: Q.V2
        a2 = V.fromList [5,0] :: Q.V2
        p1 = V.fromList [0,0] :: Q.V2
        p2 = V.fromList [1,1] :: Q.V2
        p3 = V.fromList [-2,-2] :: Q.V2
        points = V.fromList [p1, p2, p3] :: Q.VV2
    print points
    print (maxAreaPoint a1 a2 points)
    
area :: IO()
area = do
    print "tri area test"
    let a1 = V.fromList [0,0] :: Q.V2
        a2 = V.fromList [5,0] :: Q.V2
        p1 = V.fromList [1,0] :: Q.V2
        p2 = V.fromList [1,1] :: Q.V2
        p3 = V.fromList [-2,-2] :: Q.V2
    print (triArea a1 a2 p1)
    print (triArea a1 a2 p2)
    print (triArea a1 a2 p3)

--Works
group :: IO()
group = do 
    print "grouper test"
    let a1 = V.fromList [0,0] :: Q.V2
        a2 = V.fromList [0,1] :: Q.V2
        p1 = V.fromList [1,0] :: Q.V2
        p2 = V.fromList [1,1] :: Q.V2
        p3 = V.fromList [-2,-2] :: Q.V2
        points = V.fromList [p1, p2, p3] :: Q.VV2
    print (grouper a1 a2 points)

maxer :: IO()
maxer = do
    print "Max test:"
    let p1 = V.fromList [100,0] :: Q.V2
        p2 = V.fromList [1,1] :: Q.V2
        p3 = V.fromList [-2,-2] :: Q.V2
        points = V.fromList [p1, p2, p3] :: Q.VV2
    print (maxv points)

outer :: IO()
outer = do
    print "keep outer test"
    let t1 = V.fromList [0,0] :: Q.V2
        t2 = V.fromList [5,0] :: Q.V2
        t3 = V.fromList [3, 4] :: Q.V2
        p1 = V.fromList [3,5] :: Q.V2
        p2 = V.fromList [3,1] :: Q.V2
        points = V.fromList [p1, p2] :: Q.VV2
    print (keepOuter t1 t2 t3 points)

