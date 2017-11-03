module Parser (parser, parseComm) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do 
                  whiteSpace lis
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , opLetter      = char '='
                                  , reservedNames = ["true","false","skip","if",
                                                     "then","else","end", "repeat","until"]
                                  , reservedOpNames = ["+","-","*","/","~", ">", "<","?",":"]
                                  })
  
----------------------------------
--- Parser de expressiones enteras
-----------------------------------
op :: String -> a -> Parser a
op xs f = do reservedOp lis xs; return f

rsrv :: String -> a -> Parser a
rsrv xs f = do reserved lis xs; return f

intexp :: Parser IntExp
intexp  = term   `chainl1` addop
  where
    term    = factor `chainl1` mulop
    factor  = iatom <|> neg
    mulop   =   op "*" Times 
            <|> op "/" Div 
    addop   =   op "+" Plus 
            <|> op "-" Minus
    var = do v <- identifier lis
             return (Var v)
    number = do n <- natural lis
                return (Const (fromInteger n))
    neg   =  do reservedOp lis "-"
                e <- iatom
                return (UMinus e)
    iatom = parens lis intexp <|> number <|> var  

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser BoolExp
boolexp = conj `chainl1` op "|" Or
  where conj = batom `chainl1` op "&" And
        neg = do reservedOp lis "~"
                 b <- batom
                 return  (Not b)
        comp = do 
                  x <- intexp
                  rel <- op "<" Lt <|> op ">" Gt <|> op "=" Eq
                  y <- intexp
                  return (x `rel` y)
        batom =  rsrv "true" BTrue <|> rsrv "false" BFalse <|> neg <|> try (parens lis boolexp) <|> comp 

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = cmd `chainl1` op ";" Seq
   where cmd =    rsrv "skip" Skip <|> asgn <|> cond <|> loop
         asgn =   do v <- identifier lis
                     reservedOp lis ":="
                     e <- intexp
                     return (Let v e)
         cond =   do reserved lis "if"
                     b <- boolexp
                     reserved lis "then"
                     c1 <- comm
                     reserved lis "else"
                     c2 <- comm
                     reserved lis "end"
                     return (Cond b c1 c2)
         loop =   do reserved lis "repeat"
                     c <- comm
                     reserved lis "until"
                     b <- boolexp
                     reserved lis "end"
                     return (Repeat c b)

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

parser :: String -> Comm
parser s = either (error. show) id (parseComm "" s)
