module Coercion where 

    import GlobalState (TCheckState)
    import Syntax
    import Type

    import qualified GlobalState as GlobalS (newLabel)

    -- | Check if two types are consistent. 
    isConsistent :: Type -> Type -> Bool
    isConsistent Dyn _                             = True
    isConsistent _ Dyn                             = True
    isConsistent s t 
        | s == t                                   = True
    isConsistent (TRef s) (TRef t)                 = isConsistent s t
    isConsistent (Arr s1 s2) (Arr t1 t2) 
        | isConsistent t1 s1 && isConsistent s2 t2 = True
    isConsistent _ _                               = False

    -- | Check if a coercion is normalized (cannot be further reduced).
    isNormalized :: Coercion -> Bool
    -- | Base
    isNormalized (Inject _)                 = True
    isNormalized (Project _ _)              = True
    isNormalized (Iden _)                   = True
    isNormalized (Seq c (Inject _))                 
        | isNormalized c                    = True
    isNormalized (Seq (Project _ _) c)              
        | isNormalized c                    = True

    -- | Fail
    isNormalized (Seq (Project _ _) Fail{}) = True      
    isNormalized (Seq (Func c d) Fail{}) 
        | isNormalized c && isNormalized d  = True
    isNormalized (Seq (CRef c d) Fail{}) 
        | isNormalized c && isNormalized d  = True

    -- | Function
    isNormalized (Func (Iden _) (Iden _))   = False
    isNormalized (Func c d) 
        | isNormalized c && isNormalized d  = True
    isNormalized (Seq c FuncInj) 
        | isNormalized c                    = True
    isNormalized (Seq (FuncProj _) c)
        | isNormalized c                    = True

    -- | Reference
    isNormalized (CRef (Iden _) (Iden _))   = False
    isNormalized (CRef c d) 
        | isNormalized c && isNormalized d  = True
    isNormalized (Seq c RefInj)                    
        | isNormalized c                    = True
    isNormalized (Seq (RefProj _) c)
        | isNormalized c                    = True
        
    -- | Other
    isNormalized _                          = False

    -- | Check if a coercion is regular (normalized and not Identity).
    isRegular :: Coercion -> Bool
    isRegular c 
        | isNormalized c && not (isIdentity c) = True
    isRegular _                                = False

    -- | Check if a coercion is an identity.
    isIdentity :: Coercion -> Bool
    isIdentity (Iden _) = True
    isIdentity _        = False

    -- | Coerce two input types.
    coerce :: Type -> Type -> TCheckState Coercion
    coerce s1 s2  
        | s1 == s2           = return $ Iden s1                        -- ^ (C-ID)
    coerce (Arr s t) Dyn     = do c <- coerce (Arr s t) (Arr Dyn Dyn)  -- ^ (C-FUN!) 
                                  return $ Seq c FuncInj        
    coerce (TRef s) Dyn      = do c <- coerce (TRef s) (TRef Dyn)      -- ^ (C-REF!) 
                                  return $ Seq c RefInj
    coerce ty Dyn            = return $ Inject ty                      -- ^ (C-B!)
    coerce Dyn (Arr s t)     = do c <- coerce (Arr Dyn Dyn) (Arr s t)  -- ^ (C-FUN?)
                                  l <- GlobalS.newLabel
                                  return $ Seq (FuncProj l) c  
    coerce Dyn (TRef s)      = do c <- coerce (TRef Dyn) (TRef s)      -- ^ (C-REF?)
                                  l <- GlobalS.newLabel
                                  return $ Seq (RefProj l) c  
    coerce Dyn ty            = Project ty <$> GlobalS.newLabel         -- ^ (C-B?)
    coerce (Arr s1 s2) (Arr t1 t2)                                     -- ^ (C-FUN)
        | areConsistent      = do c <- coerce t1 s1
                                  d <- coerce s2 t2 
                                  return $ Func c d  
      where 
        areConsistent = Arr s1 s2 `isConsistent` Arr t1 t2
    coerce (TRef s) (TRef t)                                           -- ^ (C-REF)
        | s `isConsistent` t = do c <- coerce t s
                                  d <- coerce s t 
                                  return $ CRef c d  
    coerce s1 s2             = Fail s1 s2 <$> GlobalS.newLabel         -- ^ (C-FAIL)

    -- | Reduce a coercion (single-step).
    reduceCoercion :: Coercion -> Coercion
    reduceCoercion (Seq (Iden _) c)                 = c                           -- ^ (L-ID1)
    reduceCoercion (Seq c (Iden _))                 = c                           -- ^ (L-ID2)
    reduceCoercion (Seq (Fail s1 s2 l) _)           = Fail s1 s2 l                -- ^ (L-FAILL)
    reduceCoercion (Seq (Inject _) (Fail s1 s2 l))  = Fail s1 s2 l                -- ^ (L-FAILR)
    reduceCoercion (Seq (Inject s1) (Project s2 l)) 
        | s1 == s2                                  = Iden s1                     -- ^ (L-INJPROJ)
        | otherwise                                 = Fail s1 s2 l                -- ^ (L-FAIL)
    reduceCoercion (Func (Iden ty) (Iden _))        = Iden ty                     -- ^ (L-FUNID)
    reduceCoercion (Func (Fail s1 s2 l) _)          = Fail s1 s2 l                -- ^ (E-FUNFAILL)
    reduceCoercion (Func c (Fail s1 s2 l))
        | isNormalized c                            = Fail s1 s2 l                -- ^ (E-FUNFAILR)
    reduceCoercion (Func c d)    
        | isNormalized c                            = Func c $ reduceCoercion d   -- ^ (L-FUN1)
        | otherwise                                 = reduceCoercion c `Func` d   -- ^ (L-FUN2)
    reduceCoercion (CRef (Iden ty) (Iden _))        = Iden ty                     -- ^ (L-REFID)
    reduceCoercion (CRef (Fail s1 s2 l) _)          = Fail (TRef s1) (TRef s2) l  -- ^ (E-REFFAILL)
    reduceCoercion (CRef c (Fail s1 s2 l))
        | isNormalized c                            = Fail (TRef s1) (TRef s2) l  -- ^ (E-REFFAILR)
    reduceCoercion (CRef c d)    
        | isNormalized c                            = CRef c $ reduceCoercion d   -- ^ (L-REF1)
        | otherwise                                 = reduceCoercion c `CRef` d   -- ^ (L-REF2)
    reduceCoercion (Seq (CRef c1 c2) (CRef d1 d2))     
        | all isNormalized [c1, c2, d1, d2]         = Seq d1 c1 `CRef` Seq c2 d2  -- ^ (L-SEQREF)
    reduceCoercion (Seq (Func c1 c2) (Func d1 d2))                          
        | all isNormalized [c1, c2, d1, d2]         = Seq d1 c1 `Func` Seq c2 d2  -- ^ (L-SEQFUN)
    reduceCoercion (Seq (Seq c1 c2) c3)                        
        | isNormalized $ Seq c1 c2                  = Seq c1 $ Seq c2 c3          -- ^ (L-ASS1)
    reduceCoercion (Seq c1 (Seq c2 c3))                       
        | isNormalized $ Seq c2 c3                  = Seq c1 c2 `Seq` c3          -- ^ (L-ASS2)
    reduceCoercion (Seq c d)                                  
        | isNormalized c                            = Seq c $ reduceCoercion d    -- ^ (L-SEQ1)
        | otherwise                                 = reduceCoercion c `Seq` d    -- ^ (L-SEQ2)
    reduceCoercion c                                = c
    
    -- | Normalize a coercion (big-step reduction).
    normalize :: Coercion -> Coercion
    normalize c 
        | isNormalized c = c
        | otherwise      = normalize $ reduceCoercion c

    -- | Find the source and target type of a coercion.
    getCoercionTypes :: Coercion -> (Type, Type) 
    getCoercionTypes (Iden ty)      = (ty, ty)
    getCoercionTypes (Project ty _) = (Dyn, ty)
    getCoercionTypes (Inject ty)    = (ty, Dyn)
    getCoercionTypes (Func c1 c2)   = (Arr s1 s2, Arr t1 t2)
      where 
        (t1, s1) = getCoercionTypes c1
        (s2, t2) = getCoercionTypes c2
    getCoercionTypes (CRef _ c)     = (TRef s, TRef t)
      where 
        (s, t) = getCoercionTypes c
    getCoercionTypes (Seq c1 c2)    = (s1, t2)
      where 
        (s1, s2) = getCoercionTypes c1
        (t1, t2) = getCoercionTypes c2
    getCoercionTypes (Fail s1 s2 _) = (s1, s2)