module Chapter8.FunctionalParsers where
--see orignial
--http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf

import            Prelude  hiding (return, (>>=))
import qualified  Control.Monad as M


type Parser a = String -> [(a, String)]
-- BASIC Parsers

-- return always succeeds with the provided value
result :: a -> Parser a
result v = \inp -> [(v,inp)]

-- failure will always fail
zero :: Parser a
zero = \inp -> []

-- item: will the head of string and tail as remainder
-- if inp is empty it will fail
item :: Parser Char 
item = \inp -> case inp of
                []      -> []
                (x:xs)  -> [(x,xs)]


-- parse: parse the string using explicit application                
parse :: Parser a -> String -> [(a,String)]
parse p inp = p inp


{---------
Sequencing
----------}

seq :: Parser a -> Parser b -> Parser (a,b)
seq pa pb = \inp -> [((a,b), inp'') | (a, inp')  <- pa inp 
                                    , (b, inp'') <- pb inp']

{---------
BIND
----------}

-- pdf's parse version 
bind :: Parser a -> (a -> Parser b) -> Parser b 
p `bind` f = \inp -> concat [f v inp' | (v,inp') <- p inp]


-- the books versio nof bind
-- uses the parse function 
bind' :: Parser a -> (a -> Parser b) -> Parser b
p `bind'` f = \inp -> case p inp of 
                      []        -> []
                      [(v,out)]  -> parse (f v) out

(>>=) = bind
    

--example parse 1st char , ignore 2nd, parser 3rd
p1_3' :: Parser (Char, Char)   
p1_3' =   item >>= \c1 ->
          item >>= \_  ->
          item >>= \c2 ->
          result (c1,c2)



sat :: (Char -> Bool) -> Parser Char
sat p = item `bind` \c ->
          if p c then result c else zero


char :: Char -> Parser Char
char x = sat (\y -> y == x)

digit :: Parser Char 
digit = sat (\c -> '0' <= c && c <= '9')

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')


{-------
PLUS
choice combinator
--------}

plus :: Parser a -> Parser a -> Parser a 
p `plus` q = \inp -> (p inp ++ q inp)

letter :: Parser Char
letter = lower `plus` upper


alphanum :: Parser Char
alphanum = letter `plus` digit

word :: Parser String
word = neWord `plus` result ""
        where
            neWord = letter `bind`  \x  ->
                     word   `bind`  \xs -> 
                     result (x:xs)
                     

word' :: Parser String
word' =  letter `bind`  \x  ->
         word   `bind`  \xs -> 
         result (x:xs)


{----
This defenition can only take us this far to becoming a Monad.
In order to make Parser into a Monad it has to be a type constructor. 
Currently the def of Parser is just a type alias
`type Parser a = String -> [(a, String)]`

it will need to be changed to a newtype:
`type Parser a = P(String -> [(a, String)]`)

where `P` will be the constructor.
as a `newtype` the `P` constructor will be erased on runtime
see: http://stackoverflow.com/questions/5889696/difference-between-data-and-newtype-in-haskell#5889784

----}