module Lib where
--
import Data.List
import Data.Bool
import System.IO
import Text.Read

import Parser
import Point

getColorDistance :: Point -> Point -> Float
getColorDistance p c = do
    case p of
        Point (x1, y1) (r1, g1, b1) -> do
            let rF = fromIntegral r1
            let gF = fromIntegral g1
            let bF = fromIntegral b1
            case c of
                Centroid (r2, g2, b2) -> do
                    let d = ((rF - r2) ** 2) + ((gF - g2) ** 2) + ((bF - b2) ** 2)
                    sqrt d

forEachCluster :: Point -> [[Point]] -> Float -> Int -> Int -> Int
forEachCluster p [] min n i = n
forEachCluster p (cluster:rest) min n i = do
    let centroid = head cluster
    let d = getColorDistance p centroid
    case d < min of
        True -> forEachCluster p rest d i (i + 1)
        False -> forEachCluster p rest min n (i + 1)

addPointToCluster :: Point -> [[Point]] -> Int -> [[Point]]
addPointToCluster p (first:clusters) 0 = do
    let newCluster = first ++ [p]
    newCluster : clusters
addPointToCluster p (first:clusters) n = first : addPointToCluster p clusters (n - 1)

changeEachCentroid :: [[Point]] -> [[Point]] -> [[Point]]
changeEachCentroid [] new = new
changeEachCentroid (cluster:rest) new = do
    let startUp = Centroid (-1.0, -1.0, -1.0)
    let averageTmp = calculateAverage startUp (tail cluster) 0
    case averageTmp of
        Centroid (-1.0,-1.0,-1.0) -> do
            changeEachCentroid rest (new ++ [([(head cluster)] ++ (tail cluster))])
        Centroid (r,g,b) -> do
            changeEachCentroid rest (new ++ [([averageTmp] ++ (tail cluster))])


forEachPoint :: [[Point]] -> [Point] -> [[Point]]
forEachPoint clusters [] = do
    let result = changeEachCentroid clusters []
    result
forEachPoint clusters (p:rest) = do
    let n = forEachCluster p clusters 500 (-1) 0
    let update = addPointToCluster p clusters n
    forEachPoint update rest

getAverageChange :: [[Point]] -> [[Point]] -> Float -> Float -> Float
getAverageChange [] [] change n = change / n
getAverageChange (c1:rest1) (c2:rest2) (-1.0) n = do
    let centroid1 = head c1
    let centroid2 = head c2
    case centroid1 of
        Centroid (r1,g1,b1) -> do
            case centroid2 of
                Centroid (r2,g2,b2) -> do
                    let d = sqrt (((r1 - r2) ** 2) + ((g1 - g2) ** 2) + ((b1 - b2) ** 2))
                    getAverageChange rest1 rest2 d (n + 1)
getAverageChange (c1:rest1) (c2:rest2) change n = do
    let centroid1 = head c1
    let centroid2 = head c2
    case centroid1 of
        Centroid (r1,g1,b1) -> do
            case centroid2 of
                Centroid (r2,g2,b2) -> do
                    let d = sqrt (((r1 - r2) ** 2) + ((g1 - g2) ** 2) + ((b1 - b2) ** 2))
                    getAverageChange rest1 rest2 (d + change) (n + 1)


emptyClusters :: [[Point]] ->  [[Point]] -> [[Point]]
emptyClusters [] clusters = clusters
emptyClusters (c:rest) new = do
    let newCluster = [] ++ [[(head c)]]
    emptyClusters rest (new ++ newCluster)

whileNotClose :: [[Point]] -> [Point] -> Float -> [[Point]]
whileNotClose clusters points e = do
    let newClusters = forEachPoint clusters points
    let change = getAverageChange clusters newClusters (-1.0) 0
    case change <= e of
        True -> newClusters
        False -> do
            let empty = emptyClusters newClusters []
            whileNotClose empty points e

calculateAverage :: Point -> [Point] -> Float -> Point
calculateAverage average [] 0 = average
calculateAverage average [] n = do
    case average of
        Centroid (r,g,b) -> do
            let rFinal = r / n
            let gFinal = g / n
            let bFinal = b / n
            Centroid (rFinal,gFinal,bFinal)
calculateAverage average (p:rest) n = do
    case p of
        Point coords (rPoint,gPoint,bPoint) -> do
            case average of
                Centroid (-1.0,-1.0,-1.0) -> do
                    let rF = fromIntegral rPoint
                    let gF = fromIntegral gPoint
                    let bF = fromIntegral bPoint
                    calculateAverage (Centroid (rF,gF,bF)) rest (n + 1)
                Centroid (r,g,b) -> do
                    let rA = ((fromIntegral rPoint) + r)
                    let gA = ((fromIntegral gPoint) + g)
                    let bA = ((fromIntegral bPoint) + b)
                    calculateAverage (Centroid (rA, gA, bA)) rest (n + 1)

getMaxRed :: [Point] -> Int -> Float
getMaxRed [] max = fromIntegral max
getMaxRed (p:rest) max = do
    case p of
        Point (x,y) (r,g,b) -> do
            case r > max of
                True -> getMaxRed rest r
                False -> getMaxRed rest max

getMaxGreen :: [Point] -> Int -> Float
getMaxGreen [] max = fromIntegral max
getMaxGreen (p:rest) max = do
    case p of
        Point (x,y) (r,g,b) -> do
            case g > max of
                True -> getMaxGreen rest g
                False -> getMaxGreen rest max

getMaxBlue :: [Point] -> Int -> Float
getMaxBlue [] max = fromIntegral max
getMaxBlue (p:rest) max = do
    case p of
        Point (x,y) (r,g,b) -> do
            case b > max of
                True -> getMaxBlue rest b
                False -> getMaxBlue rest max

generateClusters :: Int -> Point -> [[Point]] -> [[Point]]
generateClusters 0 _ clusters = clusters
generateClusters k maxValues clusters = do
    case maxValues of
        Centroid (r,g,b) -> do
            let valr = r * (fromIntegral k)
            let valg = g * (fromIntegral k)
            let valb = b * (fromIntegral k)
            let to_add = Centroid (valr,valg,valb)
            generateClusters (k - 1) maxValues (clusters ++ [[to_add]])


compressIt :: Int -> Float -> [Point] -> [[Point]]
compressIt k e points = do
    let maxValues = Centroid ((getMaxRed points (-1)) / fromIntegral (k + 2),(getMaxGreen points (-1)) / fromIntegral (k + 2),(getMaxBlue points (-1)) / fromIntegral (k + 2))
    let clusters = generateClusters k maxValues []
    let newClusters = whileNotClose clusters points e
    newClusters

imageCompressor' :: Int -> Float -> String -> [[Point]]
imageCompressor' kstr estr content = do
    let list = lines content
    let points = listPoints list []
    compressIt kstr estr points