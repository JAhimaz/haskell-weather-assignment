module Main where

import Lib
import GHC.Float
import Data.List.Split ( chunksOf, splitOn )
import Data.List
import Data.Time ( defaultTimeLocale, parseTimeM, Day )
import Data.Maybe ( isJust )

-- Following the proper structure (Input -> Process -> Output)

main :: IO ()
main = fetchData >>= \input -> printOutput (process input)

-- Fetches the data by reading the CSV file

fetchData = readFile "data/datasheet.csv"

process x =
    let fixedData = (resultData x)
    -- Creates a list of days
        dayList = fixedListOfDays fixedData
        dayAverages = map (\x -> dayData x fixedData) dayList
        daysAndAverages = zip dayList dayAverages
    in 
    dayAverages

-- Puts data in its proper formatting
resultData s = map (splitOn ",") (fixedResult (lines s))

-- Drops the first 10 elements in the rawData

fixedResult :: [x] -> [x]
fixedResult  x = drop 10 x

-- Sorts the temperatures by the day in the timestamp

sortByDay y xs = [double | [date, double] <- xs, y `isPrefixOf` date] 
dayData x result = average . map toDouble . sortByDay x $ result

-- Sorting and creating List of Days

listOfDays :: [[x]] -> [x] -- Takes in a list of lists and returns the timestamp from each inner list and puts it in a single list
listOfDays xs = [date | [date, double] <- xs]
newListOfDays :: [[x]] -> [[x]] -- Removes the last 5 (Time) from the TimeStamp
newListOfDays = map (\l -> take (length l - 5) l)
unique :: Eq a => [a] -> [a] --Uniquefication 
unique x = nub x

-- Takes in the result, fixes it, takes the list of days, cuts the last few and unique-fies it.
fixedListOfDays :: Eq x => [[[x]]] -> [[x]] 
fixedListOfDays result = unique . newListOfDays . listOfDays $ result

-- Changes String to Double
toDouble :: String -> Double
toDouble = read

-- Gets Average of List of Doubles
average :: [Double] -> Double
average xs = sum xs / (int2Double $ length xs)

-- Apply a function on a list
apply :: (t -> a) -> [t] -> [a]
apply f xs = [f x | x <- xs]

printOutput x = do
    let dailyAverage = x
    let weeklyAverage = apply average (chunksOf 7 x)
    let totalAverage = average x

    print "Daily Average Temperatures"
    mapM_ print dailyAverage
    print "Weekly Average Temperatures"
    mapM_ print weeklyAverage
    print "Total Average Temperature"
    print (show totalAverage ++ " Degrees Celcius")
    
{-

------------------------------------------------

>> How to run? <<

> stack ghci
> main

Will output all the required values for the assignment

>> How does it work? <<

1.
2.
3.
4.
5.
6.
7.
8.

------------------------------------------------

Credits

Created By Joshua Daveston Ahimaz
16102543

Functional Programming Principles

------------------------------------------------

-}