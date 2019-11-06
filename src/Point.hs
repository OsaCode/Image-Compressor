module Point where
--

data Point = Point (Int, Int) (Int, Int, Int) | Centroid (Float, Float, Float) deriving (Read, Show)

parsePoint :: ReadS Point
parsePoint s = reads $ "Point " ++ s

isThisPoint :: (Int, Int) -> IO()
isThisPoint a = print(a)

listPoints :: [String] -> [Point] -> [Point]
listPoints [] list = list
listPoints (str:rest) list = do
    let parsed = parsePoint str
    case parsed of
        ((Point pt color, _):_) -> do
            let add = Point pt color
            listPoints rest (list ++ [add])
        _ -> listPoints rest list