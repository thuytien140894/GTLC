module Errors where 
    
  import Syntax
  import Types
  
  data TypeError 
    = NotBound Term 
    | Difference Type Type
    | Mismatch Type Type
    | NotBool Type
    | NotNat Type
    | NotFunction Term
    | IllegalAssign Term
    | IllegalDeref Term
    | NotRecord Term
    | InvalidLabel String
    deriving (Eq, Show)

  data RuntimeError 
    = CastError Type Type
    | InvalidRef Int
    | Stuck
    deriving (Eq, Show)