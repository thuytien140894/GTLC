module Types where

    data Type 
      = Unit
      | Bool 
      | Nat
      | Arr Type Type  
      deriving (Eq, Show)