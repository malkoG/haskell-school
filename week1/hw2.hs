import Data.List

data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

type Code = [Peg]

data Move = Move Code Int Int
          deriving (Show, Eq)

colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]


--Exercise 1
exactMatches :: Code -> Code -> Int
exactMatches [] [] = 0
exactMatches (x:xs) (y:ys) = result + exactMatches xs ys
  where result = if x == y then 1 else 0

--Exercise 2
countColors :: Code -> [Int]
countColors [] = [0]
countColors list = map (\x -> wordcount list x) colors
  where wordcount :: Code -> Peg -> Int
        wordcount [] w = 0
        wordcount (x:xs) w = (if x == w then 1 else 0) + wordcount xs w

matches :: [Peg] -> [Peg] -> Int
matches [] [] = 0
matches list1 list2 = matchNumber (countColors list1) (countColors list2) 
  where matchNumber :: [Int] -> [Int] -> Int
        matchNumber [] [] = 0
        matchNumber (x:xs) (y:ys) = (if x==y then x else min'(x,y) ) + matchNumber xs ys
          where min' :: (Ord a) => (a, a) -> a
                min' (x, y) = if x > y then y else x

--Exercise 3
getMove :: Code -> Code -> Move
getMove l1 l2 = Move l2 (exactNum l1 l2) ((matches l1 l2)-exactNum l1 l2)
  where exactNum [] [] = 0
        exactNum (x:xs) (y:ys) = (if x == y then 1 else 0) + exactNum xs ys

--Exercise 4
isConsistent :: Move -> Code -> Bool
isConsistent (Move code1 exact nonExact) code2 = (exact == exactNum code1 code2 && nonExact == (matches code1 code2) - exactNum code1 code2) 
  where exactNum [] [] = 0
        exactNum (x:xs) (y:ys) = (if x == y then 1 else 0) + exactNum xs ys

--Exercise 5
filterCodes :: Move -> [Code] -> [Code]
filterCodes m codes = filter (isConsistent m) codes

--Exercise 6
allCodes :: Int -> [Code]
