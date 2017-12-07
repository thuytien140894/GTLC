module Types where

    data Type 
      = TUnit -- trivial type
      | Bool 
      | Nat
      | Arr {argTy :: Type, retTy :: Type} 
      | TRec [(String, Type)] 
      deriving (Eq, Show)