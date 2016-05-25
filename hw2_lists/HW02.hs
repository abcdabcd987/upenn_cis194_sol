{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches xs ys = length $ filter id $ zipWith (==) xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = map (\color -> length $ filter (color ==) xs) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum $ zipWith min (countColors xs) (countColors ys)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact (total - exact)
    where exact = exactMatches secret guess
          total = matches secret guess

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess ex non) secret = (ex == ex') && (non == non')
    where (Move _ ex' non') = getMove secret guess

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = concatMap (\xs -> [color:xs | color <- colors]) $ allCodes (n-1)

-- Exercise 7 -----------------------------------------

playHistory :: Code -> [Code] -> [Move]
playHistory secret (code:[]) = [getMove secret code]
playHistory secret guesses
    | ex == (length secret)  = [move]
    | otherwise              = move : (playHistory secret guesses')
    where guess              = head guesses
          move@(Move _ ex _) = getMove secret guess
          guesses'           = filterCodes move guesses

solve :: Code -> [Move]
solve secret = playHistory secret (allCodes $ length secret)

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
