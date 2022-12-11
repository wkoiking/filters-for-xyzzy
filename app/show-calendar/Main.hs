module Main where

import ShowCalendar
import System.Environment

main :: IO ()
main = do
    filename : _<- getArgs
    showCalendar filename
