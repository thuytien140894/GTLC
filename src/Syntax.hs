{-# Language PatternSynonyms #-}

module Syntax where

  import Types

  import Data.Map (Map)
  import qualified Data.Map as Map (null, empty)

  type Entry = (String, Term)

  newtype Label = Label Int
    deriving (Eq, Show)

  newtype Store = Store (Term, Type)
    deriving (Eq, Show)

  newtype StoreEnv = StoreEnv (Map Int Store)
    deriving (Eq, Show)

  emptyStore :: StoreEnv
  emptyStore = StoreEnv Map.empty

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
    | If Term Term Term                                           -- if t then t else t
    | Succ Term                                                   -- succ t 
    | Pred Term                                                   -- pred t
    | IsZero Term                                                 -- iszero t
    | Rec [Entry]                                                 -- record
    | Proj Term String                                            -- projection
    -- | Lambda {varTy :: Type, body :: Term, boundVars :: [String]} -- abstraction
    | Lambda Type Term [String]
    | App Term Term                                               -- application 
    | Ref Term                                                    -- reference creation
    | Deref Term                                                  -- dereference (!t)
    | Loc Int                                                     -- store location
    | Assign Term Term                                            -- assignment (t := t)
    | Cast Coercion Term                                          -- coercion (<c> t)
    deriving (Eq, Show) 
  
  data Coercion 
    = Iden Type                                                   -- Identity 
    | Project Type Label                                          -- type projection (B?)
    | Inject Type                                                 -- type injection (B!)
    | CRef Coercion Coercion                                      -- reference coercion
    | Func Coercion Coercion                                      -- function coercion
    | Seq Coercion Coercion                                       -- coercion sequence (c;c)
    | Fail Type Type Label                                        -- fail
    deriving (Eq, Show)