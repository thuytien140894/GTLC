module Types where

    data Type 
      = TUnit -- trivial type
      | Top
      | Bool 
      | Nat
      | Arr Type Type
      | TRec [TEntry]  
      deriving (Eq, Show)

    type TEntry = (String, Type)