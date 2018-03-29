module Syntax where

  import Types
  
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
    deriving (Eq, Show) 
  
  data Coercion 
    = Iden Type
    | Project Type
    | Inject Type
    | Func Coercion Coercion
    | Seq Coercion Coercion
    | Fail Type Type
    deriving (Eq, Show)


  type Entry = (String, Term)
  type Label = String