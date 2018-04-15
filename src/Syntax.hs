{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}

module Syntax where

  import Types

  import Data.Map (Map)
  import qualified Data.Map as Map (null, empty)

  type Entry = (String, Term)

  newtype Label = Label Int
    deriving (Eq, Show)

  pattern EmptyStore :: StoreEnv 
  pattern EmptyStore <- StoreEnv (Map.null -> True) 
    where EmptyStore = StoreEnv Map.empty

  newtype Store = Store (Term, Type)
    deriving (Eq, Show)

  newtype StoreEnv = StoreEnv (Map Int Store)
    deriving (Eq, Show)

  pattern FuncInj :: Coercion 
  pattern FuncInj = Inject (Arr Dyn Dyn)

  pattern FuncProj :: Label -> Coercion 
  pattern FuncProj l = Project (Arr Dyn Dyn) l

  pattern RefInj :: Coercion
  pattern RefInj = Inject (TRef Dyn)

  pattern RefProj :: Label -> Coercion
  pattern RefProj l = Project (TRef Dyn) l
  
  data Term 
    = Unit                                                        -- identity term 
    | Zero                                                        -- 0
    | Tru                                                         -- true
    | Fls                                                         -- false
    -- | Var {index :: Int, ty :: Type, name :: String}              -- variable
    | Var Int Type String
    | If Term Term Term                                           -- conditional
    | Succ Term                                                   -- succ
    | Pred Term                                                   -- pred
    | IsZero Term                                                 -- iszero
    | Rec [Entry]                                                 -- record
    | Proj Term String                                            -- projection
    -- | Lambda {varTy :: Type, body :: Term, boundVars :: [String]} -- abstraction
    | Lambda Type Term [String]
    | App Term Term                                               -- application
    | Ref Term                                                    -- reference creation
    | Deref Term                                                  -- dereference
    | Loc Int                                                -- store location
    | Assign Term Term                                            -- assignment
    | Cast Coercion Term                                          -- coercion
    | Blame Label                                                 -- blame assignment
    deriving (Eq, Show) 
  
  data Coercion 
    = Iden Type
    | Project Type Label
    | Inject Type 
    | CRef Coercion Coercion
    | Func Coercion Coercion
    | Seq Coercion Coercion
    | Fail Type Type Label
    deriving (Eq, Show)