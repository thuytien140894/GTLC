module Types where

    data Type 
      = TUnit -- trivial type
      | Bool 
      | Nat
      | Arr {argTy :: Type, retTy :: Type} 
      | TRec [TEntry] 
      | Top 
      deriving (Eq, Show)

    type TEntry = (String, Type)