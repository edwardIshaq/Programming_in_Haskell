module Chapter8.FunctionalParsers where

import            Prelude  hiding (return, (>>=))
import qualified  Control.Monad as M


type Parser a = String -> [(a, String)]

-- BASIC Parsers

-- return always succeeds with the provided value
return :: a -> Parser a
return v = \inp -> [(v,inp)]

-- failure will always fail
failure :: Parser a
failure = \inp -> []

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

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \inp -> case parse p inp of 
                    [] -> []
                    [(v,out)] -> parse (f v) out
    

--example parse 1st char , ignore 2nd, parser 3rd
p1_3 :: String -> [((Char, Char), String)]
p1_3 = do c1 <- item
          item
          c2 <- item
          return (c1, c2)
   
p1_3' :: Parser (Char, Char)   
p1_3' =   item >>= \c1 ->
          item >>= \_  ->
          item >>= \c2 ->
          return (c1,c2)



--instance Monad (String -> [(a, String)]) where
--  return v = \inp -> [(v,inp)]
  
  
