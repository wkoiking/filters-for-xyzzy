module Main where

import Lib

main = do
    str <- getContents
    case parsePipeTable str of
        Nothing -> putStrLn "Can not parse pipe table"
        Just table -> putStr $ unlines $ showForest $ tableToForest table
