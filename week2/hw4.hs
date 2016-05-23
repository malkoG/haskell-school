
newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P []

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
      P list1 == P list2 = not (list1 /= list2)
      P list1 /= P list2 = not (list1 == list2)
        

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
      show list = result
        where
          result = f list 0
          
          f :: (Show a) => Poly a -> Int -> String
          f (P []) _ = ""
          f (P (x:xs)) n = eval (f (P xs) (n+1)) (order x n)
          
          eval :: String -> String -> String
          eval str1 str2
            | head str2 == '0' = str1
            | str1 == "" = str2
            | otherwise = str1 ++ " + " ++ str2
            
          order :: (Show a) => a -> Int -> String
          order x expo
            | expo == 0 = show x
            | expo == 1 = show x ++ "x"
            | otherwise = (show x) ++ "x^" ++ (show expo) 

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P list1) (P list2) = P result
  where result = addList list1 list2
        addList :: (Num a) => [a] -> [a] -> [a]
        addList [] [] = []
        addList [] (x:xs) = x:addList xs []
        addList (x:xs) [] = x:addList xs []
        addList (x:xs) (y:ys) = (x+y):addList xs ys 

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times list1 list2 = P result
  where result = mult list1 list2
        mult :: (Num a) => [a] -> [a] -> [a]
        mult list (x:xs) 

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
      (+) = plus
      (*) = times
      negate  = undefined
      fromInteger = undefined
-- No meaningful definitions exist
      abs    = undefined
      signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
      deriv  :: a -> a
      nderiv :: Int -> a -> a
      nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
      deriv = undefined
      
