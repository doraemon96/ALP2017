module Simplytyped (
       conversion,    -- conversion a terminos localmente sin nombre
       eval,          -- evaluador
       infer,         -- inferidor de tipos
       quote          -- valores -> terminos
       )
       where

import Data.List
import Data.Maybe
import Prelude hiding ((>>=))
import Text.PrettyPrint.HughesPJ (render)
import PrettyPrinter
import Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' _ LUnit          = Unit
conversion' b (LVar n)       = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (App t u)      = conversion' b t :@: conversion' b u
conversion' b (Abs n t u)    = Lam t (conversion' (n:b) u)
conversion' b (Let n u1 u2)  = LetIn (conversion' b u1) (conversion' (n:b) u2)
conversion' b (LAs u t)      = As t (conversion' b u)
conversion' b (LTuple u1 u2) = Tuple (conversion' b u1) (conversion' b u2)
conversion' b (LFst u)       = Fst (conversion' b u)
conversion' b (LSnd u)       = Snd (conversion' b u)

-----------------------
--- eval
-----------------------

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n)              = Free n
sub i t (u :@: v)             = sub i t u :@: sub i t v
sub i t (Lam t' u)            = Lam t' (sub (i+1) t u)
sub i t (LetIn u1 u2)         = LetIn (sub i t u1) (sub (i+1) t u2)
sub i t (As t' u)             = As t' (sub i t u)
sub _ _ Unit                  = Unit
sub i t (Tuple u1 u2)         = Tuple (sub i t u1) (sub i t u2)
sub i t (Fst u)               = Fst (sub i t u) --Optimizar?
sub i t (Snd u)               = Snd (sub i t u) --Optimizar?

-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ Unit                  = VUnit
eval _ (Bound _)             = error "variable ligada inesperada en eval"
eval e (Free n)              = fst $ fromJust $ lookup n e
eval _ (Lam t u)             = VLam t u
eval e (LetIn u1 u2)         = case eval e u1 of
                 VLam t u -> eval e (sub 0 (Lam t u) u2)
                 _        -> error "Error de tipo en run-time"
eval e (As t u)              = eval e u
eval e (Tuple u1 u2)         = let u' = eval e u1 in
                 VTuple u' (eval e u2)
eval e (Fst u)               = case eval e u of
                 VTuple u1 _ -> u1
                 _           -> error "Error de tipo en run-time"
eval e (Snd u)               = case eval e u of
                 VTuple _ u2 -> u2
                 _           -> error "Error de tipo en run-time"
eval e (Lam t u :@: v)       = eval e (sub 0 (quote (eval e v)) u)
eval e (u :@: v)             = case eval e u of
                 VLam t u' -> eval e (Lam t u' :@: v)
                 _         -> error "Error de tipo en run-time, verificar type checker"

-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f)     = Lam t f
quote VUnit          = Unit
quote (VTuple f1 f2) = Tuple (quote f1) (quote f2)

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=) :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 = err $ "se esperaba " ++
                         render (printType t1) ++
                         ", pero " ++
                         render (printType t2) ++
                         " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' _ _ Unit = ret TUnit
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free n) = case lookup n e of
                        Nothing -> notfoundError n
                        Just (_,t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> 
                       infer' c e u >>= \tu ->
                       case tt of
                         Fun t1 t2 -> if (tu == t1) 
                                        then ret t2
                                        else matchError t1 tu
                         _         -> notfunError tt
infer' c e (Lam t u) = infer' (t:c) e u >>= \tu ->
                       ret $ Fun t tu
infer' c e (LetIn u1 u2) = infer' c e u1 >>= \tu1 ->
                           infer' (tu1:c) e u2
infer' c e (As t u) = let t' = infer' c e u in
                        case t' of
                          Right t'' -> if t == t''
                                       then ret t
                                       else matchError t t'' --TODO: Preguntar
                          l -> l
infer' c e (Tuple u1 u2) = infer' c e u1 >>= \tu1 ->
                           infer' c e u2 >>= \tu2 ->
                            ret (TTuple tu1 tu2)
infer' c e (Fst u) = infer' c e u >>= \tt ->
                       case tt of
                        TTuple t1 _ -> ret t1
                        _           -> err "No tuple"
infer' c e (Snd u) = infer' c e u >>= \tt ->
                       case tt of
                        TTuple _ t2 -> ret t2
                        _           -> err "No tuple"
----------------------------------
