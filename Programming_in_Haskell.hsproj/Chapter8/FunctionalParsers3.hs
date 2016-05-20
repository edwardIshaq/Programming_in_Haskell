module Chapter8.FunctionalParsers3 where
--see orignial
--http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf

import qualified Control.Monad as M
import           Data.Char
import           Prelude       hiding (return, (>>=))

newtype Parser a = P(String -> [(a, String)])

-- BASIC Parsers
-- return always succeeds with the provided value
return :: a -> Parser a
return v = P(\inp -> [(v,inp)])

-- failure will always fail
failure :: Parser a
failure = P(\inp -> [])

-- item: will the head of string and tail as remainder
-- if inp is empty it will fail
item :: Parser Char
item = P(\inp -> case inp of
                []      -> []
                (x:xs)  -> [(x,xs)])


-- parse: parse the string using explicit application
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp


{---------
Sequencing
----------}

seq :: Parser a -> Parser b -> Parser (a,b)
seq (P pa) (P pb) = P(\inp -> [((a,b), inp'') | (a, inp')  <- pa inp
                                              , (b, inp'') <- pb inp'])

{---------
BIND
----------}

-- pdf's parse version
bind :: Parser a -> (a -> Parser b) -> Parser b
(P p) `bind` f = P(\inp -> concat [parse (f v) inp' | (v,inp') <- p inp])


-- the books versio nof bind
-- uses the parse function
bind' :: Parser a -> (a -> Parser b) -> Parser b
(P p) `bind'` f = P(\inp -> case p inp of
                      []          -> []
                      [(v,out)]   -> parse (f v) out)

(>>=) = bind'


--example parse 1st char , ignore 2nd, parser 3rd
p1_3' :: Parser (Char, Char)
p1_3' =   item >>= \c1 ->
          item >>= \_  ->
          item >>= \c2 ->
          return (c1,c2)



sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

-- sat p = item `bind` \c ->
--           if p c then return c else failure


char :: Char -> Parser Char
char x = sat (== x)

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)
{-------
PLUS
choice combinator
--------}

(+++) :: Parser a -> Parser a -> Parser a
(P p) +++ p2@(P q) = P(\inp -> case p inp of
                              [] -> parse p2 inp
                              [(v,out)] -> [(v,out)])


word :: Parser String
word = neWord +++ return ""
        where
            neWord = letter `bind`  \x  ->
                     word   `bind`  \xs ->
                     return (x:xs)


parseMap :: (a -> b) -> Parser a -> Parser b
parseMap f (P pa) = P(\inp -> case pa inp of
                            []          -> []
                            [(v,out)]   -> [(f v, out)])

instance Functor Parser where
  fmap = parseMap


pureParser = return

instance Monad Parser where
  return  = return
  (>>=)   = bind

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- many1 digit
         return $ read xs

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

p :: Parser [Int]
p = do  token (symbol "[")
        n  <- natural
        ns <- many (do  symbol ","
                        natural)
        symbol "]"
        return (n:ns)

-- Arithmetic Expressions
{-
expr    ::== term (+ expr | ∊)
term    ::== factor (* term | ∊)
factor  ::== (expr) | nat
nat     ::== 0 | 1 | 2 | 3 ...
-}

expr :: Parser Int
expr =
  do t <- term
    do  symbol "+"
        e <- expr
        return (t + e)
    +++ return t

term :: Parser Int
term =  do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
        +++ return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          +++ natural
