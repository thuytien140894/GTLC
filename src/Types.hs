module Types where

  type TEntry = (String, Type)
  
  data Type 
    = TUnit                                 -- identity type
    | Dyn                                   -- dynamic type
    | Top                                   -- Top
    | Bool                                  -- boolean
    | Nat                                   -- natural number
    | Arr Type Type                         -- function type
    | TRec [TEntry]                         -- record type
    | TRef Type                             -- reference type
    deriving (Eq, Show)