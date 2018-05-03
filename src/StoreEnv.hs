module StoreEnv where 

    import Error
    import Type
    import Syntax

    import Data.Map (Map)
    import qualified Data.Map as Map 
    import Data.Maybe (fromJust)
    
    -- | Entry in a store environment.
    newtype Store = Store (Term, Type)
                    deriving (Eq, Show)

    -- | Store environement.
    newtype StoreEnv = StoreEnv (Map Int Store)
                       deriving (Eq, Show)

    -- | Empty store environment.
    emptyStore :: StoreEnv
    emptyStore = StoreEnv Map.empty

    -- | Get the type of a store.
    getStoreType :: Store -> Type 
    getStoreType (Store (_, ty)) = ty

    -- | Allocate a new store.
    allocate :: StoreEnv -> Term -> (Term, StoreEnv) 
    allocate store t = (Loc l, insertRef store l v)
      where 
        l = sizeOf store
        v = Store (t, typeOf t store)

    -- | Return the number of stores. 
    sizeOf :: StoreEnv -> Int 
    sizeOf (StoreEnv s) = Map.size s

    -- | Look up the store at a location.
    lookUp :: StoreEnv -> Int -> Maybe Store
    lookUp (StoreEnv s) l = Map.lookup l s

    -- | Insert a new reference or replace an existing one.
    insertRef :: StoreEnv -> Int -> Store -> StoreEnv 
    insertRef (StoreEnv s) l t = StoreEnv $ Map.insert l t s 

    -- | Find the type for a term. 
    typeOf :: Term -> StoreEnv -> Type
    typeOf t store = case t of 
        Unit           -> TUnit                                        
        Tru            -> Bool                                          
        Fls            -> Bool                                         
        Zero           -> Nat                                           
        Succ _         -> Nat                                           
        -- Rec _          -> rcdTypeOf t store        
        Loc l          -> getStoreType $ fromJust $ store `lookUp` l                                                  
        Lambda ty t' _ -> Arr ty $ typeOf t' store 