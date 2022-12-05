module Main where

import ShowCalendar
import System.Environment


main = do
    filename : _<- getArgs
    showCalendar filename
