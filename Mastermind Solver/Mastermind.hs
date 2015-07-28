{-# OPTIONS_GHC -Wall #-}
module Mastermind where

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
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (x:xs) (y:ys)
  | x == y    = exactMatches xs ys + 1
  | otherwise = exactMatches xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors []     = [0,0,0,0,0,0]
countColors (x:xs)
  | x == Red       = zipWith (+) [1,0,0,0,0,0] (countColors xs)
  | x == Green     = zipWith (+) [0,1,0,0,0,0] (countColors xs)
  | x == Blue      = zipWith (+) [0,0,1,0,0,0] (countColors xs)
  | x == Yellow    = zipWith (+) [0,0,0,1,0,0] (countColors xs)
  | x == Orange    = zipWith (+) [0,0,0,0,1,0] (countColors xs)
  | x == Purple    = zipWith (+) [0,0,0,0,0,1] (countColors xs)
  | otherwise      = countColors xs

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches x y  = foldr (+) 0 minimums
  where minimums = (zipWith (min) (countColors x) (countColors y))

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove x y = Move y (exactMatches x y) (nonexact)
  where nonexact = matches x y - exactMatches x y

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent a@(Move guess _ _) secret = isConsistent' (getMove guess secret) a

isConsistent' :: Move -> Move -> Bool
isConsistent' (Move _ x xs) (Move _ y ys)
  | x == y && xs == ys = True
  | otherwise          = False

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes x y = filter (isConsistent x) y

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = [[Red],[Green],[Blue],[Yellow],[Orange],[Purple]]
allCodes n = addPeg (allCodes (n - 1))

addPeg :: [Code] -> [Code]
addPeg y  = concatMap (single) y
  where 
    single x = [(Red:x),(Green:x),(Blue:x),(Yellow:x),(Orange:x),(Purple:x)]


-- Exercise 7 -----------------------------------------
solve :: Code -> [Move]
solve x = tail (solve' x y (allCodes (length x)))
  where 
    y = replicate (length x) Red

solve' :: Code -> Code -> [Code] -> [Move] 
solve' x y []     = [getMove x y]
solve' x y (z:zs) = move : solve' x z (filterCodes move zs)
    where
      move = getMove x y
