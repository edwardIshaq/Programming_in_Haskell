module Chapter5.CaesarUpper where
  
import Prelude hiding (length, replicate)
import Data.Char
import Data.List hiding (length, lowers, count, replicate)

-- ENCODE

length xs = sum [1 | _ <- xs]

lowers    :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count c cs = length [x | x <- cs, x == c]

positions x xs = [ i | (x', i) <- zip xs [0..n], x' == x ]
  where n = length xs - 1
  
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let x = chr (ord 'a' + x)

upLet2int :: Char -> Int 
upLet2int c = ord c - ord 'A'

int2upper :: Int -> Char
int2upper x = chr (ord 'A' + x)

shift     :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n ) `mod` 26)
          | isUpper c = int2upper ((upLet2int c + n ) `mod` 26)
          | otherwise = c
          

encode  :: Int -> String -> String
encode n xs = [shift n x | x <- xs]


-- DECODE

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

