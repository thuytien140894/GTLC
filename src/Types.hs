module Types where

    data Type 
      = TUnit                                 -- identity type
      | Top                                   -- Top
      | Bool                                  -- boolean
      | Nat                                   -- natural number
      | Arr Type Type                         -- function type
      | TRec [TEntry]                         -- record type
      deriving (Eq, Show)

    type TEntry = (String, Type)