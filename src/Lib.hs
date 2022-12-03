module Lib where

import Data.List
import Control.Monad
-- parsec
import Text.Parsec hiding ((<|>), uncons)
import Text.Parsec.String

data Tree = Node String Forest deriving (Show)

type Forest = [Tree]

sampleStr :: String
sampleStr = unlines
    [ "* aaaaa"
    , "    * bbbb"
    , "        * ccc"
    , "        * ccc"
    , "        * ccc"
    , "    * bbb"
    , "        * ccc"
    , "        * ccc"
    , "* aaa"
    , "    * bbb"
    , "    * bbb"
    , "    * bbb"
    , "        * ccc"
    , "        * ccc"
    , "        * ccc"
    ]

interactiveIO :: ([[String]] -> String) -> IO ()
interactiveIO render = do
    str <- getContents
    case parse (parseForest 0) "" str of
        Left err -> print err
        Right trees -> putStrLn $ render $ tabulateForest trees

showPipeTable :: [[String]] -> String
showPipeTable table' = unlines $ map addBorder $ indentToToken separater table'
 where addBorder str = concat [delimiter, " ", str,  " ", delimiter]
       delimiter = "|"
       separater = concat [" ", delimiter, " "]

showTabTable :: [[String]] -> String
showTabTable rows = unlines $ map printRow rows
 where printRow as = intercalate "\t" as

parseForest :: Int -> Parser Forest
parseForest n = many $ try $ parseTree n

parseTree :: Int -> Parser Tree
parseTree n = do
    replicateM n $ string $ replicate 4 ' '
    string "* "
    str <- manyTill anyChar (try endOfLine)
    trees <- parseForest (n + 1)
    return $ Node str trees

tabulateForest :: Forest -> [[String]]
tabulateForest ts = concatMap tabulateTree ts

tabulateTree :: Tree -> [[String]]
tabulateTree (Node a []) = [[a]]
tabulateTree (Node a ts) = zipWith (:) (a : repeat "") $ tabulateForest ts

indentToToken
    :: String -- separater
    -> [[String]] -- contents
    -> [String]
indentToToken separater rows = map (intercalate separater . zipWith pad maxLengthOfEachColumn) table
 where table = map addTrailnigEmptyCellsToRow rows
       pad :: Int -> String -> String
       pad n str = take n $ str ++ repeat ' '
       addTrailnigEmptyCellsToRow :: [String] -> [String]
       addTrailnigEmptyCellsToRow row = take maxLengthOfRows $ row ++ repeat ""
       maxLengthOfRows :: Int
       maxLengthOfRows = maximum $ map length rows
       maxLengthOfEachColumn :: [Int]
       maxLengthOfEachColumn = foldl go (replicate numOfColumn 0) table
        where go xs ys = zipWith max xs (map length ys)
              numOfColumn :: Int
              numOfColumn = maximum $ map length table
