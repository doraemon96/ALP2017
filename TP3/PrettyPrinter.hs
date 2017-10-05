module PrettyPrinter (
       printTerm,     -- pretty printer para terminos
       printType,     -- pretty printer para tipos
       )
       where

import Common
import Text.PrettyPrint.HughesPJ

-- lista de posibles nombres para variables
vars :: [String]
vars = [ c : n | n <- "" : map show [(1::Integer)..], c <- ['x','y','z'] ++ ['a'..'w'] ]
              
parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp _  _  (Unit)            = text "unit"
pp ii vs (Bound k)         = text (vs !! (ii - k - 1))
pp _  _  (Free (Global s)) = text s

pp ii vs (i :@: c) = sep [parensIf (isLam i) (pp ii vs i), 
                          nest 1 (parensIf (isLam c || isApp c) (pp ii vs c))]  
pp ii vs (Lam t c) = text "\\" <>
                     text (vs !! ii) <>
                     text ":" <>
                     printType t <>
                     text ". " <> 
                     pp (ii+1) vs c
pp ii vs (LetIn c1 c2) = text "(LET " <>
                         text (vs !! ii) <>
                         text " = " <>
                         pp ii vs c1 <>
                         text " IN " <>
                         pp (ii+1) vs c2 <>
                         text ")"
pp ii vs (As t c) = text "(" <>
                    pp ii vs c <>
                    text " AS " <>
                    printType t <>
                    text ")"
pp ii vs (Tuple c1 c2) = text "(" <>
                         pp ii vs c1 <>
                         text "," <>
                         pp ii vs c2 <>
                         text ")"
pp ii vs (Fst c) = text "fst " <> 
                   pp ii vs c
pp ii vs (Snd c) = text "snd " <> 
                   pp ii vs c

isLam :: Term -> Bool                    
isLam (Lam _ _) = True
isLam  _      = False

isApp :: Term -> Bool        
isApp (_ :@: _) = True
isApp _         = False                                                               

-- pretty-printer de tipos
printType :: Type -> Doc
printType Base          = text "B"
printType TUnit         = text "Unit"
printType (TTuple t1 t2)= text "(" <>
                          printType t1 <>
                          text "," <>
                          printType t2 <>
                          text ")"
printType (Fun t1 t2)   = sep [ parensIf (isFun t1) (printType t1), 
                               text "->", 

isLam :: Term -> Bool                    
isLam (Lam _ _) = True
isLam  _      = False

isApp :: Term -> Bool        
isApp (_ :@: _) = True
isApp _         = False                                                               

-- pretty-printer de tipos
printType :: Type -> Doc
printType Base          = text "B"
printType TUnit         = text "Unit"
printType (TTuple t1 t2)= text "(" <>
                          printType t1 <>
                          text "," <>
                          printType t2 <>
                          text ")"
printType (Fun t1 t2)   = sep [ parensIf (isFun t1) (printType t1), 
                               text "->", 
                               printType t2]
isFun :: Type -> Bool
isFun (Fun _ _)        = True
isFun _                = False

fv :: Term -> [String]
fv Unit              = []
fv (Bound _)         = []
fv (Free (Global n)) = [n]
fv (t :@: u)         = fv t ++ fv u
fv (Lam _ u)         = fv u
fv (LetIn c1 c2)     = fv c1 ++ fv c2
fv (As t c)          = fv c
fv (Tuple c1 c2)     = fv c1 ++ fv c2
fv (Fst c)           = fv c
fv (Snd c)           = fv c
  
---
printTerm :: Term -> Doc 
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

