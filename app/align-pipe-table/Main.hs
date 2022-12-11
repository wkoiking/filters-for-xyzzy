module Main where

import Lib

main :: IO ()
main = do
    str <- getContents
    putStr $ case parsePipeTable str of
        Nothing -> str
        Just table -> showPipeTable table
