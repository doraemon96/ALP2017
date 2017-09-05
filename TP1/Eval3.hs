module Eval3 (eval) where

import AST
import Data.List

data Error = DivByZero | UndefVar deriving Show
type Correct a = Either Error a
type Trace = [String]
type Traced a = (Trace, Correct a)

-- Traza nula
initTrace :: [String]
initTrace = []

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
eval :: Comm -> Traced State
eval p = evalComm p initState initTrace

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> Trace -> Traced State
evalComm Skip s t = (t , Right s)
evalComm (Let v i) s t = case evalIntExp i s t of
                         (t', Right e)  -> (("Let "++(show v)++(show e)):t', update v e s)
                         (t', Left err) -> ((show err):t' , Left err)
evalComm (Seq c1 c2) s t = case evalComm c1 s t of
                           (t', Right ec1) -> case evalComm c2 ec1 t' of
                                              (t'', Right ec2) -> (("Seq":t')++t'' , Right ec2)
                                              (t'', Left err) -> ((show err):t'' , Left err)
                           (t', Left err) -> ((show err):t' , Left err)
evalComm (Cond b c1 c2) s t = case evalBoolExp b s t of
                              (t', Right eb) -> if eb then case evalComm c1 s t' of
                                                           (t'', Right ec1) -> (("IfTrue "++(show ec1)):t'' , Right ec1)
                                                           (t'', Left err) -> ((show err):t'' , Left err)
                                                      else case evalComm c2 s t' of
                                                           (t'', Right ec2) -> (("IfFalse "++(show ec2)):t'' , Right ec2)
                                                           (t'', Left err) -> ((show err):t'' , Left err)
                              (t', Left err) -> ((show err):t' , Left err)
evalComm (Repeat c b) s t = case evalComm c s t of
                            (t', Right ec) -> case evalBoolExp b ec t' of
                                              (t'', Right eb) -> if eb then (t''' , Right ec) 
                                                                       else evalComm (Repeat c b) ec t'''
                                                                 where t''' = ("Repeat "++(show ec)):t''
                                              (t'', Left err) -> ((show err):t'' , Left err)
                            (t', Left err) -> ((show err):t' , Left err)

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: IntExp -> State -> Trace -> Traced Int
evalIntExp (Const n) _ t = (("Const "++(show n)):t , Right (fromInteger n))
evalIntExp (Var v) s t = (("Var "++(show v)):t , lookfor v s)
evalIntExp (UMinus i) s t = case evalIntExp i s t of
                            (t', Right ei) -> (("UMinus "++(show ei)):t' , Right ((-1) * ei))
                            (t', Left err) -> ((show err):t' , Left err)
evalIntExp (Plus i1 i2) s t = case evalIntExp i1 s t of
                              (t', Right ei1) -> case evalIntExp i2 s t' of
                                                 (t'', Right ei2) -> (((show ei1)++" Plus "++(show ei2)):t'' , Right (ei1 + ei2))
                                                 (t'', Left err) -> ((show err):t'' , Left err)
                              (t', Left err) -> ((show err):t' , Left err)
evalIntExp (Minus i1 i2) s t = case evalIntExp i1 s t of
                             (t', Right ei1) -> case evalIntExp i2 s t' of
                                                (t'', Right ei2) -> (((show ei1)++" Minus "++(show ei2)):t'' , Right (ei1 - ei2))
                                                (t'',Left err) -> ((show err):t'' , Left err)
                             (t', Left err) -> ((show err):t' , Left err)
evalIntExp (Times i1 i2) s t = case evalIntExp i1 s t of
                              (t', Right ei1) -> case evalIntExp i2 s t' of
                                                 (t'', Right ei2) -> (((show ei1)++" Times "++(show ei2)):t'' , Right (ei1 * ei2))
                                                 (t'',Left err) -> ((show err):t'' , Left err)
                              (t', Left err) -> ((show err):t' , Left err)
evalIntExp (Div i1 i2) s t = case evalIntExp i1 s t of
                             (t', Right ei1) -> case evalIntExp i2 s t' of
                                                (t'', Right 0) -> (((show ei1)++" DivByZero"):t'', Left DivByZero)
                                                (t'', Right ei2) -> (((show ei1)++" Div "++(show ei2)):t'',Right (div ei1 ei2))
                                                (t'', Left err) -> ((show err):t'',Left err)
                             (t', Left err) -> ((show err):t', Left err)

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Trace -> Traced Bool
evalBoolExp BTrue _ t = ("True":t , Right True)
evalBoolExp BFalse _ t = ("False":t , Right False)
evalBoolExp (Eq i1 i2) s t = case evalIntExp i1 s t of
                             (t', Right ei1) -> case evalIntExp i2 s t' of
                                                 (t'', Right ei2) -> (((show ei1)++" Eq "++(show ei2)):t'' , Right (ei1 == ei2))
                                                 (t'', Left err) -> ((show err):t'' , Left err)
                             (t', Left err) -> ((show err):t' , Left err)
evalBoolExp (Lt i1 i2) s t = case evalIntExp i1 s t of
                             (t', Right ei1) -> case evalIntExp i2 s t' of
                                                 (t'', Right ei2) -> (((show ei1)++" Lt "++(show ei2)):t'' , Right (ei1 < ei2))
                                                 (t'', Left err) -> ((show err):t'' , Left err)
                             (t', Left err) -> ((show err):t' , Left err)
evalBoolExp (Gt i1 i2) s t = case evalIntExp i1 s t of
                             (t', Right ei1) -> case evalIntExp i2 s t' of
                                                 (t'', Right ei2) -> (((show ei1)++" Gt "++(show ei2)):t'' , Right (ei1 > ei2))
                                                 (t'', Left err) -> ((show err):t'' , Left err)
                             (t', Left err) -> ((show err):t' , Left err)
evalBoolExp (And b1 b2) s t = case evalBoolExp b1 s t of
                              (t', Right eb1) -> case evalBoolExp b2 s t' of
                                                  (t'', Right eb2) -> (((show eb1)++" And "++(show eb2)):t'' , Right (eb1 && eb2))
                                                  (t'', Left err) -> ((show err):t'' , Left err)
                              (t', Left err) -> ((show err):t' , Left err)
evalBoolExp (Or b1 b2) s t = case evalBoolExp b1 s t of
                             (t', Right eb1) -> case evalBoolExp b2 s t' of
                                                 (t'', Right eb2) -> (((show eb1)++" Or "++(show eb2)):t'' , Right (eb1 || eb2))
                                                 (t'', Left err) -> ((show err):t'' , Left err)
                             (t', Left err) -> ((show err):t' , Left err)
evalBoolExp (Not b) s t = case evalBoolExp b s t of
                          (t', Right eb) -> (("Not "++(show eb)):t' , Right (not eb))
                          (t', Left err) -> ((show err):t' , Left err)
