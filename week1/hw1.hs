--Exercise 1
lastDigit     :: Integer -> Integer
lastDigit num = mod num 10  

dropLastDigit :: Integer -> Integer
dropLastDigit num = div num 10

--Exercise 2
toRevDigits :: Integer -> [Integer]
toRevDigits num
  | num > 0 = (lastDigit num):(toRevDigits (dropLastDigit num))
  | otherwise = []

--Exercise 3
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = calc
  where isIndexEven l = (length l) `mod` 2  /= 0
        doubleEvenIndex [] = []
        doubleEvenIndex l  = (if isIndexEven l then (head l) * 2 else (head l)):doubleEvenIndex (tail l)
        calc = doubleEvenIndex list

--Exercise 4
sumDigits :: [Integer] -> Integer
sumDigits list = sum (map (sum . toRevDigits) list)

--Exercise 5
luhn :: Integer -> Bool
luhn n
  | n  > 0 = if sumDigits (doubleEveryOther (toRevDigits  n) ) `mod` 10 == 0 then True else False
  | otherwise = error "zero or negative number"

--Exercise 6
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (n-1) c b a)

--Exercise 7
hanoi2 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi2 0 _ _ _ _ = []
hanoi2 1 a b _ _ = [(a,b)]
hanoi2 n a b c d = (hanoi2 k a d b c) ++ (hanoi (n-k) a b c) ++ (hanoi2 k d b a c)
  where k = (round . sqrt . fromInteger) (2*n+1) - 1 
