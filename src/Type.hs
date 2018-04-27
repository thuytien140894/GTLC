module Type where

    type TEntry = (String, Type)
    
    data Type = TUnit          -- ^ Unit
              | Dyn            -- ^ Dynamic
              | Top            -- ^ Top
              | Bool           -- ^ Boolean
              | Nat            -- ^ Natural number
              | Arr Type Type  -- ^ Function type
              | TRec [TEntry]  -- ^ Record type
              | TRef Type      -- ^ Reference type
                deriving (Eq, Show)