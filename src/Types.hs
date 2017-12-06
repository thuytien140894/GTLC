module Types where

    data Type 
      = TUnit -- trivial type
      | Bool 
      | Nat
      | Arr Type Type  
      deriving (Eq, Show)