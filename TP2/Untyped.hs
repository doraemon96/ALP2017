module Untyped where

import Control.Monad
import Data.List

import Common


----------------------------------------------
-- Seccón 2 - Representación de Términos Lambda 
-- Ejercicio 2: Conversión de Términos
----------------------------------------------
           
conversion  :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' l (LVar x) = case elemIndex x l of
                         Just n  -> Bound n
                         Nothing -> Free x
conversion' l (App p q) = (conversion' l p) :@: (conversion' l q)
conversion' l (Abs s p) = Lam (conversion' (s:l) p)

  
-------------------------------
-- Sección 3 - Evaluación
-------------------------------

shift :: Term -> Int -> Int -> Term
shift = undefined
  
  
subst :: Term -> Term -> Int -> Term
subst = undefined	


eval :: NameEnv Term -> Term -> Term
eval = undefined
    
    
    
    
    
