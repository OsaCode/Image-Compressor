module Parser where
--
import Data.Char

import Point

isFloat :: String -> Bool
isFloat str =
    case reads str :: [(Float, String)] of
        [(_, "")] -> True
        _ -> False

parseInt :: String -> Int
parseInt "" = -1
parseInt n = do
    case all isDigit n of
        True -> case (read n) > 0 of
            True -> (read n)
            False -> -1
        False -> -1

parseFloat :: String -> Float
parseFloat "" = -1
parseFloat n = do
    case isFloat n of
        True -> case (read n :: Float) > 0 of
            True -> (read n)
            False -> -1
        False -> -1