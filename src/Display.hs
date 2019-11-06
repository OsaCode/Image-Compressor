module Display where
--
import Text.Printf

import Point

printUsage :: IO()
printUsage = do
    putStrLn("USAGE: ./imageCompressor n e IN\n")
    putStrLn("\tn\tnumber of colors in the final image")
    putStrLn("\te\tconvergence limit")
    putStrLn("\tIN\tpath to the file containing the colors of the Pixels")

printCluster :: [Point] -> Int -> IO()
printCluster (c:cluster) 0 = do
    case c of
        Centroid (r,g,b) -> do
            putStr(printf "--\n(%.2f,%.2f,%.2f)\n" r g b)
            case cluster of
                [] -> putStrLn("-")
                _ -> do
                    putStrLn("-")
                    printCluster cluster 1
printCluster (p:[]) i = do
    case p of
        Point (x,y) (r,g,b) -> do
            putStr(printf "(%d,%d) (%d,%d,%d)\n" x y r g b)
printCluster (p:cluster) i = do
    case p of
        Point (x,y) (r,g,b) -> do
            putStr(printf "(%d,%d) (%d,%d,%d)\n" x y r g b)
    printCluster cluster (i + 1)

displayClusters :: [[Point]] -> Int -> IO()
displayClusters (last:[]) _ = printCluster last 0
displayClusters (cluster:list) i = do
    printCluster cluster 0
    displayClusters list 1