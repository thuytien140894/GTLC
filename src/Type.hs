module Type where

    -- | Alias for record entry
    type TEntry = (String, Type)
    
    -- | Types
    data Type = TUnit          -- ^ Unit
              | Dyn            -- ^ Dynamic
              | Top            -- ^ Top
              | Bool           -- ^ Boolean
              | Nat            -- ^ Natural number
              | Arr Type Type  -- ^ Function type
              | TRec [TEntry]  -- ^ Record type
              | TRef Type      -- ^ Reference type
                deriving (Eq, Show)