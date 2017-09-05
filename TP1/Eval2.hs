module Eval1 (eval) where

import AST
import Data.List

data Error = DivByZero | UndefVar
data Correct a = Either Error a

-- Estados
type State = [(Variable,Int)]

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variable en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Correct Int
lookfor v s = if (varExist == 1)
              then Right (snd $ head $ filter ((==v).fst) s)
              else Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Int -> State -> State
update v i s = case find ((==v).fst) s of
               Just _  -> map (\x -> if fst(x) == v then (v,i) else x) s
               Nothing -> (v,i):s

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> State
evalComm Skip s = s
evalComm (Let v i) s = update v (evalIntExp i s) s
evalComm (Seq c1 c2) s = evalComm c2 (evalComm c1 s)
evalComm (Cond b c1 c2) s = if evalBoolExp b s then evalComm c1 s else evalComm c2 s
evalComm (Repeat c b) s = if evalBoolExp b s' then s' else evalComm (Repeat c b) s'
                          where s' = evalComm c s

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> Correct Int
evalIntExp (Const n) _ = Right (fromInteger n)
evalIntExp (Var v) s = lookfor v s
evalIntExp (UMinus i) s = Right ((-1) * (evalIntExp i s))
evalIntExp (Plus i1 i2) s = Right ((evalIntExp i1 s) + (evalIntExp i2 s))
evalIntExp (Minus i1 i2) s = Right ((evalIntExp i1 s) - (evalIntExp i2 s))
evalIntExp (Times i1 i2) s = Right ((evalIntExp i1 s) * (evalIntExp i2 s))
evalIntExp (Div i1 i2) s = if dividendo == 0 
                           then Left DivByZero
                           else Right (div (evalIntExp i1 s) dividendo)
                           where dividendo = evalIntExp i2 s

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Correct Bool
evalBoolExp BTrue _ = Right True
evalBoolExp BFalse _ = Right False
evalBoolExp (Eq i1 i2) s = Right ((evalIntExp i1 s) == (evalIntExp i2 s))
evalBoolExp (Lt i1 i2) s = Right ((evalIntExp i1 s) < (evalIntExp i2 s))
evalBoolExp (Gt i1 i2) s = Right ((evalIntExp i1 s) > (evalIntExp i2 s))
evalBoolExp (And b1 b2) s = Right ((evalBoolExp b1 s) && (evalBoolExp b2 s))
evalBoolExp (Or b1 b2) s = Right ((evalBoolExp b1 s) || (evalBoolExp b2 s))
evalBoolExp (Not b) s = Right (not (evalBoolExp b s))
