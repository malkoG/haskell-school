import Data.List

--Problem 1
myLast :: [a] -> a
myLast []   = error "empty list is not allowed"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

--Problem 2
elementAt :: [a] -> Int -> a
elementAt [] x = error "list is empty or index is too large"
elementAt list 0 = head list
elementAt (x:xs) index = elementAt xs (index-1)

--Problem 3
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome a = a == (reverse a) 

--Problem 4
compress :: Eq a => [a] -> [a]
compress [] = error "the list is empty." 
compress xs = map head (group xs)

--Problem 5
encode :: Eq a => [a] -> [(Int, a)]
encode [] = error "the list is empty"
encode xs = map (\x -> (length x ,head x)) (group xs)
