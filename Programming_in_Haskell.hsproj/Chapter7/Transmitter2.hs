module Chapter7.Transmitter2 where
  
import Data.Char

type Bit = Int

bin2int' :: [Bit] -> Int
bin2int' xs = sum [b * w | (b,w) <- zip xs weights]
             where weights = iterate (*2) 1
             
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 xs = take 8 (xs ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord) 

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmitt :: String -> String
transmitt = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- Add a parity bit | bit == 1 in case odd number of ones | otherwise 0

parity :: [Bit] -> Bit
parity bits =  (sum $ filter (==1) bits) `mod` 2

parityExtend :: [Bit] -> [Bit]
parityExtend xs = xs ++ [parity xs]

-- Generic chop n
chop :: Int -> [Bit] -> [[Bit]]
chop _ [] = []
chop n bits = take n bits : chop n (drop n bits)

chop9 = chop 9

pencode :: String -> [Bit]
pencode = concat . map (parityExtend . make8 . int2bin . ord)

pdecode :: [Bit] -> String
pdecode = map (chr . bin2int . parityCheck) . chop9

parityCheck :: [Bit] -> [Bit]
parityCheck [] = []
parityCheck bits = case last bits == (parity . init $ bits ) of
                        True ->  init bits
                        False -> error " "


choppyChannel :: [Bit] -> [Bit]
choppyChannel = tail


transmitt2 :: String -> String
transmitt2 = pdecode . channel . pencode

transmittChppy :: String -> String 
transmittChppy = pdecode . choppyChannel . pencode
