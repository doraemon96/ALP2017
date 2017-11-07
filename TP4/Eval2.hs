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
newtype StateError a = StateError { runStateError :: Env -> Either Error (a, Env) }

instance Monad StateError where
    return x = StateError (\e -> Right (x,e))
    m >>= f  = StateError (\e -> either (\x -> Left x) (\(x,e')-> runStateError (f x) e') (runStateError m e) )

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
    lookfor v  = StateError (\e -> maybe (Left (UndefVar v)) (\x -> Right (x,e)) (lookup v e))
    update v i = StateError (\e -> Right ((), update' v i e))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ee) | v == u = (v, i):ee
                       update' v i ((u, j):ee) | v /= u = (u, j):(update' v i ee)



-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: Error -> m a

instance MonadError StateError where
    throw err = StateError (\s -> Left err)

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval p = either Left (Right . snd) (runStateError (evalComm p) initState)

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m) => Comm -> m ()
evalComm (Skip)         = return ()
evalComm (Let v e)      = do e' <- evalIntExp e
                             update v e'
evalComm (Seq c1 c2)    = do evalComm c1
                             evalComm c2
evalComm (Cond b c1 c2) = do b' <- evalBoolExp b
                             if b' then evalComm c1 else evalComm c2
evalComm (Repeat c b)   = do evalComm c
                             b' <- evalBoolExp b
                             if b' then return () else evalComm (Repeat c b)

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: (MonadState m, MonadError m) => IntExp -> m Int
evalIntExp (Const n)     = return n
evalIntExp (Var v)       = lookfor v
evalIntExp (UMinus e)    = do e' <- evalIntExp e
                              return (-e')
evalIntExp (Plus e1 e2)  = do e1' <- evalIntExp e1
                              e2' <- evalIntExp e2
                              return (e1' + e2')
evalIntExp (Minus e1 e2) = do e1' <- evalIntExp e1
                              e2' <- evalIntExp e2
                              return (e1' - e2')
evalIntExp (Times e1 e2) = do e1' <- evalIntExp e1
                              e2' <- evalIntExp e2
                              return (e1' * e2')
evalIntExp (Div e1 e2)   = do e1' <- evalIntExp e1
                              e2' <- evalIntExp e2
                              if e2' == 0 then throw DivByZero
                                          else return (div e1' e2')

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: (MonadState m, MonadError m) => BoolExp -> m Bool
evalBoolExp (BTrue)     = return True
evalBoolExp (BFalse)    = return False
evalBoolExp (Eq e1 e2)  = do e1' <- evalIntExp e1
                             e2' <- evalIntExp e2
                             return (e1' == e2')
evalBoolExp (Lt e1 e2)  = do e1' <- evalIntExp e1
                             e2' <- evalIntExp e2
                             return (e1' < e2')
evalBoolExp (Gt e1 e2)  = do e1' <- evalIntExp e1
                             e2' <- evalIntExp e2
                             return (e1' > e2')
evalBoolExp (And b1 b2) = do b1' <- evalBoolExp b1
                             b2' <- evalBoolExp b2
                             return (b1' && b2')
evalBoolExp (Or b1 b2)  = do b1' <- evalBoolExp b1
                             b2' <- evalBoolExp b2
                             return (b1' || b2')
evalBoolExp (Not b)     = do b' <- evalBoolExp b
                             return (not b')
