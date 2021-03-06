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
shift (Bound n) c d = if n < c
                      then (Bound n)
                      else (Bound (n+d))
shift (Free x) c d  = Free x
shift (p :@: q) c d = (shift p c d) :@: (shift q c d)
shift (Lam p) c d   = Lam (shift p (c+1) d)
  
  
subst :: Term -> Term -> Int -> Term
subst (Bound n) r i = if n == i
                      then r
                      else (Bound n)
subst (Free x) r i  = Free x
subst (p :@: q) r i = (subst p r i) :@: (subst q r i)
subst (Lam p) r i   = Lam (subst p (shift r 0 1) (i+1))


eval :: NameEnv Term -> Term -> Term
eval env ((Lam p) :@: q)   = eval env (shift (subst p (shift q 0 1) 0) 0 (-1))
eval env (p :@: q)         = case eval env p of
                             (Lam r) -> eval env ((Lam r) :@: q)
                             x       -> x :@: (eval env q)
eval env (Free x)          = case lookup x env of
                             Just y  -> eval env y 
                             Nothing -> (Free x)
eval env (Bound x)         = Bound x
eval env (Lam p)           = Lam (eval env p)
