module Chapter7.Excercise  
where
  
mapFilter f p xs = [f x | x <- xs, p x ]
mapFilter' f p xs = map f (filter p xs)

all' :: (a -> Bool) -> [a] -> Bool
all' p [] =  True
all' p (x:xs) |   p x = all' p xs
              |   otherwise = False
              
all'' p = foldr (\x xs -> p x && xs) True

factors x = [ factor | factor <- [1..x] , x `mod` factor  == 0]

isPrime x = factors x == [1,x]

any' p = foldr (\x xs -> p x || xs) False

takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' p = foldr (\x xs -> 
  case p x of 
     True ->  x : xs
     otherwise -> []) 
     []


takeWhile'' :: (a->Bool) -> [a] -> [a]
takeWhile'' p [] = []
takeWhile'' p (x:xs) = if p x then x : (takeWhile'' p xs) else []


takeWhile''' :: (a->Bool) -> [a] -> [a]
takeWhile''' p = foldr (\x xs -> if p x then x : xs else []) []

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p xs@(x:xs') = case p x of
                        True -> dropWhile' p xs'
                        False -> xs
                        
map' :: (a->b) -> [a] -> [b]
map' f =  foldr (\x xs -> f x : xs) []
                        
filter' :: (a->Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

dec2int :: [Int] -> Int
dec2int = foldl (\xs x -> x + 10 * xs) 0

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f = \x -> (\y -> f(x,y))

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(x,y) -> f x y

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x  | p x = []
                | otherwise = h x : unfold p h t (t x)
                
type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)


chop8' = unfold (null) (take 8) (drop 8)

map'' f = unfold (null) (f . head) (tail)

iterate' f = unfold (\_ -> False) (id) (f)


