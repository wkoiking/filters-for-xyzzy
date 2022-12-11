module Lib where

import Data.Char
import Data.List
import Control.Monad
-- parsec
import Text.Parsec hiding ((<|>), uncons)
import Text.Parsec.String
-- split
import Data.List.Split (splitOn)

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

toCamelStyle :: String -> String
toCamelStyle = concat . map captalize . words
 where captalize "" = ""
       captalize (x : xs) = toUpper x : xs

addLabels :: String -> String
addLabels = unlines . map addLabelLine . lines

addLabelLine :: String -> String
addLabelLine line = do
    case parse parseLineForLabel "" line of
        Left _ -> line
        Right l -> unwords [line, l]

parseLineForLabel :: Parser String
parseLineForLabel = do
    (sec, title) <- choice [parseSection, parseTable, parseFigure]
    return $ concat ["{#", sec, ":", toCamelStyle title, "}"]

parseSection :: Parser (String, String)
parseSection = do
    _ <- many1 $ char '#'
    _ <- many1 $ char ' '
    title <- many1 $ noneOf "{"
    eof
    return ("sec", title)

parseTable :: Parser (String, String)
parseTable = do
    _ <- string ":"
    _ <- many1 $ char ' '
    title <- many1 $ noneOf "{"
    eof
    return ("tbl", title)

parseFigure :: Parser (String, String)
parseFigure = do
    _ <- string "!["
    title <- many1 $ noneOf "]"
    _ <- string "]("
    _path <- many $ noneOf ")"
    _ <- string ")"
    _ <- many $ char ' '
    eof
    return ("fig", title)

samplePipeTable :: String
samplePipeTable = case parse (parseForest 0) "" sampleStr of
    Left err -> show err
    Right trees -> showPipeTable $ tabulateForest trees

interactiveIO :: ([[String]] -> String) -> IO ()
interactiveIO render = do
    str <- getContents
    putStr $ case parse (parseForest 0) "" $ removeNullLines str of
        Left _ -> str
        Right trees -> render $ tabulateForest trees

removeNullLines :: String -> String
removeNullLines = unlines . filter (not .null) . lines

showPipeTable :: [[String]] -> String
showPipeTable table = unlines $ addRuledLine $ map addBorder $ indentToToken separater $ addRowForRuledLine table
 where addBorder str = concat [delimiter, " ", str,  " ", delimiter]
       addRuledLine (r1:r2:rs) = r1 : map spaceToHyphen r2 : rs
       addRuledLine rs = rs
       spaceToHyphen ' ' = '-'
       spaceToHyphen x   = x
       separater = concat [" ", delimiter, " "]
       addRowForRuledLine (r1:r2:rs) = r1:[]:r2:rs
       addRowForRuledLine rs = rs

showTabTable :: [[String]] -> String
showTabTable rows = unlines $ map printRow rows
 where printRow as = intercalate "\t" as

parsePipeTable :: String -> Maybe [[String]]
parsePipeTable table = liftM removeRuledLine $ mapM parsePipeTableRow $ lines table
 where removeRuledLine :: [[String]] -> [[String]]
       removeRuledLine (r1:r2:rs)
           | all isRuledLine r2 = r1:rs
        where isRuledLine xs = all ('-' ==) xs && not (null xs)
       removeRuledLine rs = rs

parsePipeTableRow :: String -> Maybe [String]
parsePipeTableRow row
    | delimiter `isPrefixOf` row
    , delimiter `isSuffixOf` row = Just $ removeTrailingCells $ map removePreceedingAndTrailingSpace $ splitOn delimiter $ removeBorder row
    | otherwise = Nothing
 where removeBorder = drop (length delimiter) . take (length row - length delimiter)
       removeTrailingCells cells = reverse $ dropWhile null $ reverse cells
       removePreceedingAndTrailingSpace str = dropWhile isSpace $ reverse $ dropWhile isSpace $ reverse str

delimiter :: String
delimiter = "|"

tableToForest :: [[String]] -> Forest
tableToForest [] = []
tableToForest table = map tableToTree $ spanToTables table
 where spanToTables :: [[String]] -> [[[String]]]
       spanToTables [] = []
       spanToTables (r:rs) = case span hasEmptyHead rs of
           (_, []) -> [r:rs]
           (rs', remain) -> (r:rs') : spanToTables remain
       hasEmptyHead []     = False
       hasEmptyHead (c:_) = all isSpace c

tableToTree :: [[String]] -> Tree
tableToTree rows = Node (head $ head rows) $ tableToForest $ filter (not . null) $ map (drop 1) rows

showForest :: Forest -> [String]
showForest ts = concatMap showTree ts

showTree :: Tree -> [String]
showTree (Node str ts) = ("* " ++ str) : (map (replicate 4 ' ' ++) $ showForest ts)

parseForest :: Int -> Parser Forest
parseForest n = many $ try $ parseTree n

parseTree :: Int -> Parser Tree
parseTree n = do
    replicateM_ n $ string $ replicate 4 ' '
    _ <- string "* "
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
 where table = map addTrailingEmptyCellsToRow rows
       pad :: Int -> String -> String
       pad n str = take n $ str ++ repeat ' '
       addTrailingEmptyCellsToRow :: [String] -> [String]
       addTrailingEmptyCellsToRow row = take maxLengthOfRows $ row ++ repeat ""
       maxLengthOfRows :: Int
       maxLengthOfRows = maximum $ map length rows
       maxLengthOfEachColumn :: [Int]
       maxLengthOfEachColumn = foldl go (replicate numOfColumn 0) table
        where go xs ys = zipWith max xs (map length ys)
              numOfColumn :: Int
              numOfColumn = maximum $ map length table
