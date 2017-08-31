module Eval1 (eval) where

import AST

-- Estados
type State = [(Variable,Int)]

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variable en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Int
lookfor v = snd $ head $ filter ((==v).fst)

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Int -> State -> State
update v i = map (\x -> if fst(x) == v then (v,i) else (x,snd(x))

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> State
evalComm Skip = id
evalComm (Let v i) = update v (evalIntExp i)
evalComm (Seq c1 c2) s = evalComm c2 (evalComm c1 s)
evalComm (Cond b c1 c2) s = if evalBoolExp b s then evalComm c1 s else evalComm c2 s
evalComm (Repeat c b) s = if evalBoolExp b s' then s' else evalComm (Repeat c b) s'
                          where s' = evalComm c s

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> Int
evalIntExp (Const n) _ = n
evalIntExp (Var v) = lookfor v
evalIntExp (UMinus i) s = (*(-1)) . (evalIntExp i s)
evalIntExp (Plus i1 i2) s = (evalIntExp i1 s) + (evalIntExp i2 s)
evalIntExp (Minus i1 i2) s = (evalIntExp i1 s) - (evalIntExp i2 s)
evalIntExp (Times i1 i2) s = (evalIntExp i1 s) * (evalIntExp i2 s)
evalIntExp (Div i1 i2) s = div (evalIntExp i1 s) (evalIntExp i2 s)

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp BTrue _ = true
evalBoolExp BFalse _ = false
evalBoolExp (Eq i1 i2) s = (evalIntExp i1 s) == (evalIntExp i2 s)
evalBoolExp (Lt i1 i2) s = (evalIntExp i1 s) < (evalIntExp i2 s)
evalBoolExp (Gt i1 i2) s = (evalIntExp i1 s) > (evalIntExp i2 s)
evalBoolExp (And b1 b2) s = (evalBoolExp b1 s) && (evalBoolExp b2 s)
evalBoolExp (Or b1 b2) s = (evalBoolExp b1 s) || (evalBoolExp b2 s)
evalBoolExp (Not b) s = not (evalBoolExp b s)
