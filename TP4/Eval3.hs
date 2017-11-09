module Eval3 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

-- Estados
type Env = [(Variable,Int)]

-- Traza
data CommLog = SkipL | LetL | SeqL | CondL | RepeatL deriving Show

-- Error
data Error = DivByZero| UndefVar Variable
instance Show Error where
  show DivByZero = "Error: División por cero."
  show (UndefVar v) = "Error: Variable " ++ show v ++ " indefinida."

-- Monada Log
newtype StateErrorLog a = StateErrorLog {runSEL :: Env -> (Either Error (a, Env), [CommLog])}

instance Monad StateErrorLog where
    return x = StateErrorLog (\e -> (Right (x, e),[]))
    m >>= f  = StateErrorLog (\e -> case runSEL m e of (Left err, l)     -> (Left err, l)
                                                       (Right (x,e'), l) -> (fst p, snd p ++ l) where p = runSEL (f x) e')

-- Para calmar a Mauro
instance Functor StateErrorLog where
    fmap = liftM

instance Applicative StateErrorLog where
    pure  = return
    (<*>) = ap


-- Clase MonadLog
class Monad m => MonadLog m where
    mlog :: CommLog -> m ()

instance MonadLog StateErrorLog where 
    mlog x = StateErrorLog (\e -> (Right ((),e), [x]))

-- Clase MonadError
class Monad m => MonadError m where
    -- Lanza un error
    throw :: Error -> m a

instance MonadError StateErrorLog where
    throw err = StateErrorLog (\e -> (Left err,[]))

--Clase MonadState
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

instance MonadState StateErrorLog where
    lookfor v  = StateErrorLog (\e -> maybe (Left (UndefVar v),[]) (\x -> (Right (x,e), [])) (lookup v e))
    update v i = StateErrorLog (\e -> (Right ((), update' v i e), []))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ee) | v == u = (v, i):ee
                       update' v i ((u, j):ee) | v /= u = (u, j):(update' v i ee)

-- Estado y Log nulo
initState :: Env
initState = []

-- Evalua un programa en el estado nulo
eval :: Comm -> (Either Error Env, [CommLog])
eval p = (either Left (Right . snd) (fst ans), snd ans) where ans = runSEL (evalComm p) initState

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m, MonadLog m) => Comm -> m ()
evalComm (Skip)         = do mlog SkipL
                             return ()
evalComm (Let v e)      = do e' <- evalIntExp e
                             mlog LetL
                             update v e'
evalComm (Seq c1 c2)    = do evalComm c1
                             mlog SeqL
                             evalComm c2
evalComm (Cond b c1 c2) = do b' <- evalBoolExp b
                             mlog CondL
                             if b' then evalComm c1 else evalComm c2
evalComm (Repeat c b)   = do evalComm c
                             mlog RepeatL
                             b' <- evalBoolExp b
                             if b' then return () else evalComm (Repeat c b)

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: (MonadState m, MonadError m, MonadLog m) => IntExp -> m Int
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
evalBoolExp :: (MonadState m, MonadError m, MonadLog m) => BoolExp -> m Bool
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
