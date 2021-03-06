module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement

data Statement = Assignment String Expr.T 
    | Skip 
    | Begin [Statement] 
    | If Expr.T Statement Statement 
    | While Expr.T Statement
    | Read String
    | Write Expr.T
    | Comment String
    deriving Show

exprParse = (Expr.parse :: String -> Maybe (Expr.T, String))

assignment, skip, begin, ifstm, while, readstm, write, comment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

begin = accept "begin" -# iter parse #- require "end" >-> Begin

ifstm = accept "if" -# exprParse #- require "then" # parse #- require "else" # parse >-> buildIfstm
buildIfstm ((ifstm1, ifsT), ifsF) = If ifstm1 ifsT ifsF

while = accept "while" -# exprParse #- require "do" # parse >-> buildWhile
buildWhile (v, s) = While v s

readstm = accept "read" -# word #- require ";" >-> Read

write = accept "write" -# Expr.parse #- require ";" >-> Write

comment = accept "--" -# dropComment #- require "\n" >-> Comment 

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (Assignment var ex: stm) dict input = 
    exec stm (Dictionary.insert (var, Expr.value ex dict) $ dict) input

exec (Skip: stm) dict input = 
    exec stm dict input

exec (Begin stms: stm) dict input = 
    exec (stms ++ stm) dict input

-- exec (Begin stm1: stm) =
--     exec ((stm1 ++ stm) dict input)

exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (While cond st : stm) dict input = 
    if (Expr.value cond dict) > 0 then 
        exec (st : (While cond st) : stm) dict input
    else 
        exec stm dict input

exec (Read s : stm) dict (x : input) = 
    exec stm (Dictionary.insert (s, x) $ dict) input

exec (Write ex : stm) dict input = 
    (Expr.value ex dict) : exec stm dict input

exec (Comment s : stm) dict input = 
    exec stm dict input

exec [] _ _ = []

indent n = (replicate (2 * n) ' ')

shw :: Int -> T -> String 
shw n (Assignment v e) = indent n ++ v ++ " := " ++ Expr.toString e ++ ";\n"
shw n (Skip) = indent n ++ "skip;\n"
shw n (Begin stms) = indent n ++ "begin\n" ++ concatMap (shw (n+1)) stms ++ indent n ++ "end"
shw n (If cond thenStmts elseStmts) = 
    indent n ++ "if " ++ Expr.toString cond ++ " then\n" ++ shw (n+1) thenStmts ++ indent n ++ "else\n" ++ shw (n+1) elseStmts
shw n (While cond stms) = indent n ++ "while " ++ Expr.toString cond ++ " do\n" ++ shw (n+1) stms ++ "\n"
shw n (Read s) = indent n ++ "read " ++ s ++ ";\n" 
shw n (Write s) = indent n ++ "write " ++ toString s ++ ";\n"
shw n (Comment s) = indent n ++ "--" ++ s ++ "\n"

instance Parse Statement where
  parse = assignment ! skip ! begin ! ifstm ! while ! readstm ! write
  toString = shw 0
