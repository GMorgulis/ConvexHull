# convex-hull
The command below runs the program on x threads and prints runtime statistics
stack run -- +RTS -Nx -s -ls -RTS <input-file>
The command below allows the program to be run once on all threads from 1 to 8
./batchrunner.sh <input-file>


Qhpar.hs and Qhseq.hs were our initial implementations of QuickHull. However, they are should 
not be used because the rest of the project has greatly changed since they were created. "Andrew.hs" was
obtained from https://en.wikibooks.org/wiki/Algorithm_Implementation/Geometry/Convex_hull/Monotone_chain,
and it is the correct Haskell implemenation of the Monotone Chain Algorithm; we used to this to check the validity of our answers.