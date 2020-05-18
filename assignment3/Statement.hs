module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement

{- 
    program ::= statements
    statement ::= variable ':=' expr ';'
        | 'skip' ';'
        | 'begin' statements 'end'
        | 'if' expr 'then' statement 'else' statement
        | 'while' expr 'do' statement
        | 'read' variable ';'
        | 'write' expr ';'
    statements ::= {statement}
    variable ::= letter {letter}
-}
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

assignment, skip, begin, ifstm, while, readstm, write :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin stms = Begin stms

ifstm = accept "if" -# exprParse #- require "then" # parse #- require "else" # parse #- require ";" >-> buildIfstm
buildIfstm ((ifstm1, ifsT), ifsF) = If ifstm1 ifsT ifsF

while = accept "while" -# exprParse #- require "do" # parse >-> buildWhile
buildWhile (v, s) = While v s

readstm = accept "read" -# word #- require ";" >-> buildRead
buildRead w = Read w

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e

-- comment = accept "--" -# iter (char )


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (Assignment var ex: stm) dict input = 
    exec stm (Dictionary.insert (var, Expr.value ex dict) $ dict) input

exec (Skip: stm) dict input = 
    exec stm dict input

exec (Begin stms: stm) dict input = 
    execMany stms dict input

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
    (Expr.value ex dict) : (exec stm dict input)
     
execMany :: [Statement] -> Dictionary.T String Integer -> [Integer] -> [Integer]
execMany [] _ _ = []
execMany (stm:stms) dict input = 
    (exec [stm] dict input) ++ (execMany stms dict input)

instance Parse Statement where
  parse = assignment ! skip ! begin ! ifstm ! while ! readstm ! write
  toString = error "Statement.toString not implemented"
