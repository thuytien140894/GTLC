module Errors where 
    
  import Syntax
  import Types
  
  data Cause 
    = FunArg
    | FunRet
    | RefRead 
    | RefWrite
    | Function 
    | Reference
    | None
    deriving (Eq, Show)

  data BlameRes = BlameRes Cause Term 
    deriving (Eq, Show)

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
    = CastError Type Type BlameRes
    | Blame Type Type Label Term
    | InvalidRef Int
    | Stuck
    deriving (Eq, Show)