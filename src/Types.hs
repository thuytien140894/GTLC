module Types where

    data Type 
      = Bool 
      | Nat
      | Type Type  
      deriving (Eq, Show)