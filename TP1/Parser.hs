module Parser where

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
                                                     "then","else","end", "while","do"]
                                  })
  
----------------------------------
-- Funciones auxiliares
----------------------------------

nsymbol :: String -> Parser String
nsymbol = symbol lis

nnatural :: Parser Integer
nnatural = natural lis

nidentifier :: Parser String
nidentifier = identifier lis

parseOp :: String -> (a -> b -> c) -> Parser (a -> b -> c)
parseOp s op = nsymbol s >> return op

-----------------------------------
-- Parser de expressiones enteras
-----------------------------------

intexp :: Parser IntExp
intexp  = --try
            do{ x <- nnatural ; return $ Const x }
        <|> do{ v <- nidentifier ; return $ Var v }
        <|> do{ nsymbol "-" ; i <- intexp ; return $ UMinus i}
        <|> intexp `chainl1` addop
        <|> intexp `chainl1` mulop

addop, mulop :: Parser (IntExp -> IntExp -> IntExp)
addop =   (parseOp "+" Plus)  <|> (parseOp "-" Minus)
mulop =   (parseOp "*" Times) <|> (parseOp "/" Div)

-----------------------------------
-- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser BoolExp
boolexp = undefined

-----------------------------------
-- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = undefined

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

