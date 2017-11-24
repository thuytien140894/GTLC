module Types where

    data Type 
      = Void -- "empty" type
      | Bool 
      | Nat
      | Arr Type Type  
      deriving (Eq, Show)