module Syntax where

  import Types
  
  type Entry = (String, Term)
  type LabelIndex = Int

  data Term 
    = Unit                                                        -- identity term 
    | Zero                                                        -- 0
    | Tru                                                         -- true
    | Fls                                                         -- false
    -- | Var {index :: Int, ty :: Type, name :: String}              -- variable
    | Var Int Type String
    | If Term Term Term                                           -- conditional
    | Succ Term                                                   -- succ
    | Pred Term                                                   -- pred
    | IsZero Term                                                 -- iszero
    | Rec [Entry]                                                 -- record
    | Proj Term String                                            -- projection
    -- | Lambda {varTy :: Type, body :: Term, boundVars :: [String]} -- abstraction
    | Lambda Type Term [String]
    | App Term Term                                               -- application
    | Cast Coercion Term                                          -- coercion
    | Blame LabelIndex                                            -- blame assignment
    deriving (Eq, Show) 
  
  data Coercion 
    = Iden Type
    | Project Type LabelIndex
    | Inject Type 
    | Func Coercion Coercion
    | Seq Coercion Coercion
    | Fail Type Type LabelIndex
    deriving (Eq, Show)