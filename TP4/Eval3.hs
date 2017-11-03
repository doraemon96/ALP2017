module Eval3 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

-- Estados
type Env = [(Variable,Int)]

-- Traza
data CommLog = IfLog | Cosas

-- Error
data Error = DivByZero| UndefVar Variable

-- Monada Log
newtype StateErrorLog a = StateErrorLog {runSEL :: Env -> [CommLog] -> Either Error (a,Env, [CommLog])}

instance Monad StateErrorLog where
    return x = StateErrorLog (\e l -> Right (x, e, l))
    m >>= f  = StateErrorLog (\e l -> either Left (\(x,e,l) -> runSEL (f x) e l) (runSEL m e l))

-- Para calmar a Mauro
instance Functor StateError where
    fmap = liftM

instance Applicative StateError where
    pure  = return
    (<*>) = ap

-- Estado nulo
initState :: Env
initState = []

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval = undefined

-- Evalua un comando en un estado dado
evalComm :: Comm -> m ()
evalComm = undefined

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: IntExp -> m Int
evalIntExp = undefined

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: BoolExp -> m Bool
evalBoolExp = undefined
