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
          deriving (Show, Eq)

  -- Valores
  data Value = VLam Type Term 
             | VTuple Term Term
             | VUnit 

  -- Contextos del tipado
  type Context = [Type]
