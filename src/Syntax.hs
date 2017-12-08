module Syntax where

  import Types
  
  -- declaring a new data type
  -- the new values of the type are called constructors
  -- values with arguments are called constructor functions, which are themselves fully evaluated
  data Term 
    = Unit -- trivial term
    | Zero
    | Tru
    | Fls
    | Var {index :: Int, ty :: Type, name :: String}
    | If Term Term Term
    | Succ Term
    | Pred Term
    | IsZero Term
    | Rec [Entry]
    | Proj Term String
    | Lambda {varTy :: Type, body :: Term, boundVars :: [String]} -- abstraction
    | App Term Term -- application
    deriving (Eq, Show) -- make the data type an instance of these built-in classes

  type Entry = (String, Term)