module Eval2 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

--Mensajes de error
data Error = DivByZero | UndefVar Variable

instance Show Error where
  show DivByZero = "Error: División por cero."
  show (UndefVar v) = "Error: Variable " ++ show v ++ " indefinida."

-- Mónada estado
newtype StateError a = StateError { runStateError :: Env -> Either Er { runStateError :: Env -> Either Error (a, Env) }
ror (a, Env) }

instance Monad StateError where
    return x           = StateError (\e -> Right (x,e))
    StateError r >>= f = StateError (\e -> case runStateError r s of
                                           Left  err    = Left err
                                           Right (x,e') = runStateError (f x) e')

-- Para calmar al GHC
instance Functor StateError where
    fmap = liftM
 
instance Applicative StateError where
    pure   = return
    (<*>)  = ap      


-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

instance MonadState StateError where
    lookfor v  = StateError (\e -> (lookfor' v e, e))
                 where lookfor' v ((u,j):ee) | v == u = j
                                             | v /= u = lookfor' v ee
                /\TODO:COMPLETAR CON EITHER (este es el del Eval1)/\
    update v i =


-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: Error -> m a

instance MonadError StateError where
    throw err = StateError (\s -> Left err)

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval = undefined

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m) => Comm -> m ()
evalComm = undefined

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: (MonadState m, MonadError m) => IntExp -> m Int
evalIntExp = undefined

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: (MonadState m, MonadError m) => BoolExp -> m Bool
evalBoolExp = undefined
