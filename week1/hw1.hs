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
--doubleEveryOther :: [Integer] -> [Integer]
--doubleEveryOther list = [ if 

--Exercise 4
sumDigits :: [Integer] -> Integer
sumDigits list = sum (map (sum . toRevDigits) list))

--Exercise 5
--luhn :: Integer -> Bool


--Exercise 6


--Exercise 7
