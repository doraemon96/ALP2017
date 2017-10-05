module Common where

  -- Comandos interactivos o de archivos
  data Stmt i = Def String i           --  Declarar un nuevo identificador x, let x = t
              | Eval i                 --  Evaluar el término
    deriving (Show)
  
  instance Functor Stmt where
    fmap f (Def s i) = Def s (f i)
    fmap f (Eval i)  = Eval (f i)

  -- Tipos de los nombres
  data Name
     =  Global  String
    deriving (Show, Eq)

  -- Entornos
  type NameEnv v t = [(Name, (v, t))]

  -- Tipo de los tipos
  data Type = Base 
            | TUnit
            | TTuple Type Type
            | Fun Type Type
            | Nat
            deriving (Show, Eq)
  
  -- Términos con nombres
  data LamTerm  =  LVar String
                |  Abs String Type LamTerm
                |  App LamTerm LamTerm
                |  Let String LamTerm LamTerm
                |  LAs LamTerm Type
                |  LUnit
                |  LTuple LamTerm LamTerm
                |  LFst LamTerm
                |  LSnd LamTerm
                |  LZero
                |  LSuc LamTerm
                |  R LamTerm LamTerm LamTerm
                deriving (Show, Eq)


  -- Términos localmente sin nombres
  data Term  = Bound Int
             | Free Name 
             | Term :@: Term
             | Lam Type Term
             | LetIn Term Term
             | As Type Term
             | Unit
             | Tuple Term Term
             | Fst Term
             | Snd Term
             | Zero
             | Suc Term
             | R Term Term Term
          deriving (Show, Eq)

  -- Valores
  data Value = VLam Type Term 
             | VTuple Value Value
             | VUnit
             | VNat NatValue
  data NatValue = Zero
                | Suc NatValue

  -- Contextos del tipado
  type Context = [Type]
