module Chapter5.Chapter5 where
  
import Prelude hiding (length, replicate)
import Data.Char
import Data.List hiding (length, lowers, count, replicate)

-- Numeric functions

factors n = [x | x <- [1..n], n `mod` x == 0]

isPrime n = factors n == [1,n]

primes n = [x | x <- [2..n], isPrime x]

-- Arrays functions
conca xss = [x | xs <- xss, x <- xs]

length xs = sum [1 | _ <- xs]

lowers    :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count c cs = length [x | x <- cs, x == c]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

isSorted xs = and [a <= b | (a,b) <- pairs xs]

positions x xs = [ i | (x', i) <- zip xs [0..n], x' == x ]
  where n = length xs - 1
  
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let x = chr (ord 'a' + x)

shift     :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n ) `mod` 26)
          | otherwise = c
          

encode  :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
  where n = lowers xs
  

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [ (o - e)^2 / e | (o,e) <- zip os es]

rotate  :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs


letters :: [Char]
letters = ['a'..'z']

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]


crack :: String -> String
crack xs = encode (-factor) xs
  where 
    table'  = freqs xs
    chitabs = [chisqr (rotate n table') table | n <- [0..25]]
    factor  = head (positions (minimum chitabs) chitabs)



unwrap :: Maybe a -> a -> a
unwrap Nothing defaultValue = defaultValue
unwrap (Just x) _ = x


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


