module Eval2 (eval) where

import AST
import Data.List

data Error = DivByZero | UndefVar deriving Show
type Correct a = Either Error a

-- Estados
type State = [(Variable,Int)]

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variable en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Correct Int
lookfor v s = case lookup v s of
              Just x -> Right x
              Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Int -> State -> Correct State
update v i s = case lookfor v s of
               Right _  -> Right (map (\x -> if fst(x) == v then (v,i) else x) s)
               Left _ -> Right ((v,i):s)

-- Evalua un programa en el estado nulo
eval :: Comm -> Correct State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> Correct State
evalComm Skip s = Right s
evalComm (Let v i) s = case evalIntExp i s of
                       Right e  -> update v e s
                       Left err -> Left err
evalComm (Seq c1 c2) s = case evalComm c1 s of
                         Right ec1 -> evalComm c2 ec1
                         Left err -> Left err
evalComm (Cond b c1 c2) s = case evalBoolExp b s of
                            Right eb -> if eb then evalComm c1 s else evalComm c2 s
                            Left err -> Left err
evalComm (Repeat c b) s = case evalComm c s of
                          Right ec -> case evalBoolExp b ec of
                                      Right eb -> if eb then Right ec else evalComm (Repeat c b) ec
                                      Left err -> Left err
                          Left err -> Left err

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> Correct Int
evalIntExp (Const n) _ = Right (fromInteger n)
evalIntExp (Var v) s = lookfor v s
evalIntExp (UMinus i) s = case evalIntExp i s of
                          Right ei -> Right ((-1) * ei)
                          Left err -> Left err
evalIntExp (Plus i1 i2) s = case evalIntExp i1 s of
                            Right ei1 -> case evalIntExp i2 s of
                                         Right ei2 -> Right (ei1 + ei2)
                                         Left err -> Left err
                            Left err -> Left err
evalIntExp (Minus i1 i2) s = case evalIntExp i1 s of
                             Right ei1 -> case evalIntExp i2 s of
                                          Right ei2 -> Right (ei1 - ei2)
                                          Left err -> Left err
                             Left err -> Left err
evalIntExp (Times i1 i2) s = case evalIntExp i1 s of
                             Right ei1 -> case evalIntExp i2 s of
                                          Right ei2 -> Right (ei1 * ei2)
                                          Left err -> Left err
                             Left err -> Left err
evalIntExp (Div i1 i2) s = case evalIntExp i1 s of
                           Right ei1 -> case evalIntExp i2 s of
                                        Right 0 -> Left DivByZero
                                        Right ei2 -> Right (div ei1 ei2)
                                        Left err -> Left err
                           Left err -> Left err

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Correct Bool
evalBoolExp BTrue _ = Right True
evalBoolExp BFalse _ = Right False
evalBoolExp (Eq i1 i2) s = case evalIntExp i1 s of
                           Right ei1 -> case evalIntExp i2 s of
                                        Right ei2 -> Right (ei1 == ei2)
                                        Left err -> Left err
                           Left err -> Left err
evalBoolExp (Lt i1 i2) s = case evalIntExp i1 s of
                           Right ei1 -> case evalIntExp i2 s of
                                        Right ei2 -> Right (ei1 < ei2)
                                        Left err -> Left err
                           Left err -> Left err
evalBoolExp (Gt i1 i2) s = case evalIntExp i1 s of
                           Right ei1 -> case evalIntExp i2 s of
                                        Right ei2 -> Right (ei1 > ei2)
                                        Left err -> Left err
                           Left err -> Left err
evalBoolExp (And b1 b2) s = case evalBoolExp b1 s of
                            Right eb1 -> case evalBoolExp b2 s of
                                        Right eb2 -> Right (eb1 && eb2)
                                        Left err -> Left err
                            Left err -> Left err
evalBoolExp (Or b1 b2) s = case evalBoolExp b1 s of
                           Right eb1 -> case evalBoolExp b2 s of
                                        Right eb2 -> Right (eb1 || eb2)
                                        Left err -> Left err
                           Left err -> Left err
evalBoolExp (Not b) s = case evalBoolExp b s of
                        Right eb -> Right (not eb)
                        Left err -> Left err
