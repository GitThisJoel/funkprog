module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-), dropComment) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #-

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

cons(a, b) = a:b

(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd

(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst

spaces :: Parser String -- String -> Maybe (String, String)
spaces = iter (char ? isSpace)

noEmpty :: Parser String 
noEmpty = (char ? isSpace) # iter (char ? isSpace) >-> cons

comment :: Parser String 
comment = (chars 2 ? (== "--")) -# (iter (char ? (/='\n'))) #- require "\n"

begoneCommentOrSpace :: Parser String 
begoneCommentOrSpace = (iter (comment ! noEmpty)) >-> concat

token :: Parser a -> Parser a
token m = m #- begoneCommentOrSpace

letter :: Parser Char -- String -> Maybe (Char, String)
letter = char ? isAlpha

word :: Parser String
word = token (letter # iter letter >-> cons)

dropComment :: Parser String 
dropComment s = return (dropWhile (/= '\n') s) ""

chars :: Int -> Parser String
chars n
  | n == 0 = return []
  | otherwise = char # chars (n-1) >-> cons

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

require :: String -> Parser String -- String -> String -> Maybe
require w = accept w ! err ("Program error: expecting " ++ w)

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char
digit = char ? isDigit

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')
