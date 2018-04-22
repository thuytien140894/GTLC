module Utils where

  import Syntax
  import Types
  import Errors

  import qualified Data.Map as Map 
  import Data.Maybe
  import Control.Applicative ((<|>))

  -- get the type of a store
  getStoreType :: Store -> Type 
  getStoreType (Store (_, ty)) = ty

  -- allocate a new store
  allocate :: StoreEnv -> Term -> (Term, StoreEnv) 
  allocate store t = (Loc l, insertRef store l v)
    where l  = sizeOf store
          v  = Store (t, typeOf t store)

  -- return the number of stores 
  sizeOf :: StoreEnv -> Int 
  sizeOf (StoreEnv s) = Map.size s

  -- look up the store at a location
  lookUp :: StoreEnv -> Int -> Maybe Store
  lookUp (StoreEnv s) l = Map.lookup l s

  -- insert a new reference or replace an existing one
  insertRef :: StoreEnv -> Int -> Store -> StoreEnv 
  insertRef (StoreEnv s) l t = StoreEnv $ Map.insert l t s 

  -- update the value at a store location
  updateStore :: StoreEnv -> Int -> Term -> Either RuntimeError (Term, StoreEnv) 
  updateStore store l t = case store `lookUp` l of 
                            Just (Store (_, ty)) -> let v = Store (t, ty) 
                                                    in Right (t, insertRef store l v) 
                            Nothing              -> Left $ InvalidRef l

  -- add new entry to the record
  addField :: Term -> (String, Term) -> Term
  addField (Rec ls) newField = Rec (newField : ls)

  -- add new entry to the record type
  addType :: Type -> (String, Type) -> Type
  addType (TRec ls) newType = TRec (newType : ls)

  -- add new entry to the record
  addEntry :: Term -> Entry -> Term
  addEntry (Rec ls) newElem = Rec (newElem : ls)

  -- find the type for a record
  rcdTypeOf :: Term -> StoreEnv -> Type
  rcdTypeOf (Rec []) store                = TRec []
  rcdTypeOf (Rec ((l1, t1) : ys)) store   = rcdTypeOf (Rec ys) store `addType` (l1, ty)
    where ty = typeOf t1 store
    
  -- find the type for a term 
  typeOf :: Term -> StoreEnv -> Type
  typeOf t store = case t of 
    Unit               -> TUnit                                        
    Tru                -> Bool                                          
    Fls                -> Bool                                         
    Zero               -> Nat                                           
    Succ _             -> Nat                                           
    Rec _              -> rcdTypeOf t store        
    Loc l              -> getStoreType $ fromJust $ store `lookUp` l                                                  
    Lambda ty t' _     -> Arr ty $ typeOf t' store 

  -- renumber the indices of free variables in a term
  -- maintain the "cutoff" parameter c that controls which variables should be shifted
  -- i.e. variables with indices less than c are bound and therefore should stay the same
  shift :: Int -> Int -> Term -> Term
  shift a b t = case t of 
    Var k ty id      -> if k < a then t else Var (k + b) ty id
    Cast c t'        -> Cast c $ shift a b t'
    Succ t'          -> Succ $ shift a b t'
    Pred t'          -> Pred $ shift a b t'
    IsZero t'        -> IsZero $ shift a b t'
    If t1 t2 t3      -> let t1' = shift a b t1
                            t2' = shift a b t2 
                            t3' = shift a b t3
                        in If t1' t2' t3'
    Lambda ty t' ctx -> Lambda ty (shift (a + 1) b t') ctx
    Ref t'           -> Ref $ shift a b t'
    Deref t'         -> Deref $ shift a b t'
    Assign t1 t2     -> shift a b t1 `Assign` shift a b t2
    App t1 t2        -> shift a b t1 `App` shift a b t2
    _                -> t -- t is a constant
    
  -- perform substitution given a variable with bruijn index j, a body s, and a term t
  subs :: Int -> Term -> Term -> Term 
  subs j s t = case t of 
    Var k ty id      -> if k == j then s else t
    Cast c t'        -> Cast c $ subs j s t'
    Succ t'          -> Succ $ subs j s t'
    Pred t'          -> Pred $ subs j s t'
    IsZero t'        -> IsZero $ subs j s t'
    If t1 t2 t3      -> let t1' = subs j s t1
                            t2' = subs j s t2 
                            t3' = subs j s t3
                        in If t1' t2' t3'
    Lambda ty t' ctx -> Lambda ty (subs (j + 1) (shift 0 1 s) t') ctx
    Ref t'           -> Ref $ subs j s t'
    Deref t'         -> Deref $ subs j s t'
    Assign t1 t2     -> subs j s t1 `Assign` subs j s t2
    App t1 t2        -> subs j s t1 `App` subs j s t2
    _                -> t -- t is a constant