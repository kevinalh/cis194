{-# OPTIONS_GHC -Wall #-}
module HW02 where
import Data.List

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
exactMatches (x:xs) (y:ys) = (if x==y then 1 else 0) + exactMatches xs ys
exactMatches _ _ = 0


-- Exercise 2 -----------------------------------------

-- For each color, count how many times it's repeated in xs
countColors :: Code -> [Int]
countColors xs = map (countAColor xs) colors
    where countAColor :: [Peg] -> Peg -> Int
          countAColor ys color = length (filter (color==) ys)

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = foldl' (+) 0 (zipWith min (countColors xs) (countColors ys))

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess (exactMatches secret guess) (matches secret guess - exactMatches secret guess)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess x y) secret = getMove secret guess == Move guess x y

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m = filter (isConsistent m)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n
    | n <= 0 = [[]]
    | otherwise = concatMap newCodes (allCodes (n-1))
        where newCodes :: Code -> [Code]
              newCodes a = map (:a) colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = nextMove filteredCodes
    where initialGuess = replicate (length secret) Red
          filteredCodes = filterCodes (getMove secret initialGuess) (allCodes (length secret))
          nextMove :: [Code] -> [Move]
          nextMove [] = []
          nextMove (guess:availableCodes)
            | guess==secret = [getMove secret guess]
            | otherwise = getMove secret guess : nextMove availableCodes

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
