module Chapter5.Excercise where
  
import Prelude hiding (length, replicate)
import Data.Char
import Data.List hiding (length, lowers, count, replicate)


factors n = [x | x <- [1..n], n `mod` x == 0]

length xs = sum [1 | _ <- xs]

positions x xs = [ i | (x', i) <- zip xs [0..n], x' == x ]
  where n = length xs - 1
  

-- Excercises

exc1 = sum [ x^2 | x <- [1..100]]

replicate :: Int -> a -> [a]
replicate n x = [ x |  _ <- take n [1..]]


pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- range, y <- range, z <- range , x^2 + y^2 == z^2]
  where range = [1..n]
  

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n] , isPerfect x]
  where isPerfect x = x == sum (init (factors x))

v1 = [(x,y) | x <- [1,2,3] , y <- [4,5,6]]
v2 = concat [[(x,y) | y <- [4..6]] | x <- [1..3]]
exc5 = v1 ==  v2


scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [ x*y | (x,y) <- zip xs ys]