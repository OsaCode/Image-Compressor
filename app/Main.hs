module Main where

import System.Environment
import System.Exit
import System.Directory

import Lib
import Display
import Parser

imageCompressor :: Int -> Float -> String -> IO()
imageCompressor (-1) _ _ = printUsage
imageCompressor _ (-1) _ = printUsage
imageCompressor kstr estr path = do
    isAFile <- doesFileExist path
    case (isAFile) of
        True -> do
            content <- readFile path
            displayClusters (imageCompressor' kstr estr content) 0
        False-> printUsage

parseCommand :: [String] -> IO()
parseCommand args = do
    case args of
        [] -> printUsage
        [k, e, path] -> imageCompressor (parseInt k) (parseFloat e) path
        _ -> printUsage

main :: IO()
main = do
    args <- getArgs
    parseCommand args