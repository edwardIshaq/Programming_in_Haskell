module Chapter7.HigherExamples where

import Prelude
import Data.Char

doubleMap = map (map (*2)) [[10..20], [11,22..50]]
  
map1 :: (a -> b) -> [a] -> [b]
map1 f xs = [ f x | x <- xs]

map2 :: (a -> b) -> [a] -> [b]
map2 _ [] = []
map2 f (x:xs) = f x : map f xs

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p xs = [ x | x <- xs, p x]

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 p (x:xs) | p x        = x : filter2 p xs
                 | otherwise  = filter2 p xs


length' = foldl (\n _ -> n + 1) 0

-- [a] -> [a]
reverse' = foldl (\xs x -> x : xs) []
reverse'' = foldr (\x xs -> xs ++ [x]) []
--(xs ++) = foldl (\ys y -> ys ++ [y]) xs


-- COMPOSITION 7.5

f :: Int -> Char
f x = chr x


g :: Char -> String
g c = [c]

(•) = (.)

odd' = not • even
twice f = f • f

compose :: [a->a] -> (a->a)
compose = foldr (•) id
