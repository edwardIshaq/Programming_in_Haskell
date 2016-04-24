module Chapter6.Excercise where
 
import Prelude hiding (exp, and, concat, replicate, (!!), elem, merge, sum)
  

exp :: Integral a => a -> a -> a
x `exp` 0 = 1
a `exp` b = a * (a `exp` (b-1))


{- 
2 exp 3
2 * (2 exp 2)
2 * (2  * (2 exp 1))
2 * (2  * (2 * (2 exp 0)))
2 * (2  * (2 * (1)))
-}

and :: [Bool] -> Bool
and [] = True
and (True:xs) = and xs
and (False:xs) = False

concat :: [[a]] -> [a]
concat [xs] = xs
concat (xs:xss) = xs ++ concat xss

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(_:xs) !! n = xs !! (n-1)


elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem y (x:xs) | y == x = True
              | otherwise = elem y xs
              

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge a@(x:xs) b@(y:ys) | x <= y = [x] ++ merge xs b
                        | otherwise = [y] ++ merge a ys
                          
                        
halve :: [a] -> ([a], [a])
halve xs = (take p xs, drop p xs) where p = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right) 
  where (left, right) = halve xs
  

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = x : take (n-1) xs

last' :: [a] -> a
last' [] = error "`last` only works on non-empty list"
last' [a] = a
last' (_:xs) = last' xs
