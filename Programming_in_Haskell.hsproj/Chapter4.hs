module Chapter4 where
  
import Prelude 

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
  where n = (length xs) `div` 2
  
-- safetail using conditional expression
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

-- safetail using guarded expression
safetail2 :: [a] -> [a]
safetail2 xs  | null xs = []
              | otherwise = tail xs
              
-- safetail using pattern matching
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (x:xs) = xs


or1 :: Bool -> Bool -> Bool
or1 True   True  = True
or1 True   False = True
or1 False  True  = True
or1 False  False = False

or2 :: Bool -> Bool -> Bool
or2 True   _  = True
or2 _  True  = True
or2 False  False = False

or3 :: Bool -> Bool -> Bool
or3 False  False = False
or3 _ _ = True

or4 :: Bool -> Bool -> Bool
or4 False  b  = b
or4 True   _ = True


and1 :: Bool -> Bool -> Bool 
and1 b1 b2 = if (b1 == True) then 
              if (b2 == True) then True else False 
              else False
--and1 b1 b2 = if (b1 == True && b2 == True) then True else False

and2 :: Bool -> Bool -> Bool 
and2 b1 b2 = if (b1 == True) then b2 else False


mult x y z = x * y * z

mult2 = (\x -> (\y -> (\z -> x * y * z)))
