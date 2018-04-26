module Coercion where 

  import Syntax
  import Types

  -- increment the label index
  increment :: Label -> Label
  increment (Label l) = Label $ l + 1

  -- consistency rules
  isConsistent :: Type -> Type -> Bool
  isConsistent Dyn _                               = True
  isConsistent _ Dyn                               = True
  isConsistent s1 s2 | s1 == s2                    = True
  isConsistent (Arr s1 s2) (Arr t1 t2) 
    | isConsistent t1 s1 &&
      isConsistent s2 t2                           = True
  isConsistent _ _                                 = False

  -- check if a coercion is normalized (cannot be further reduced)
  isNormalized :: Coercion -> Bool
  -- Base
  isNormalized (Inject _)                         = True
  isNormalized (Project _ _)                      = True
  isNormalized (Iden _)                           = True
  isNormalized (Seq c (Inject _))                 
    | isNormalized c                              = True
  isNormalized (Seq (Project _ _) c)              
    | isNormalized c                              = True
  -- Fail
  isNormalized (Seq (Project _ _) Fail{})         = True      
  isNormalized (Seq (Func c d) Fail{}) 
    | isNormalized c && isNormalized d            = True
  -- Function
  isNormalized (Func (Iden _) (Iden _))           = False
  isNormalized (Func c d) 
    | isNormalized c && isNormalized d            = True
  isNormalized (Seq c FuncInj) 
    | isNormalized c                              = True
  isNormalized (Seq (FuncProj _) c)
    | isNormalized c                              = True
  -- Reference
  isNormalized (CRef (Iden _) (Iden _))           = False
  isNormalized (CRef c d) 
    | isNormalized c && isNormalized d            = True
  isNormalized (Seq c RefInj)                    
    | isNormalized c                              = True
  isNormalized (Seq (RefProj _) c)
    | isNormalized c                              = True
  -- Other
  isNormalized _                                  = False

  -- check if a coercion is regular (normalized and not Identity or Fail)
  isRegular :: Coercion -> Bool
  isRegular c 
    | isNormalized c && not (isIdentity c)       = True
  isRegular _                                    = False

  -- check if a coercion is an identity
  isIdentity :: Coercion -> Bool
  isIdentity (Iden _) = True
  isIdentity _        = False

  -- coercion type system
  coerce :: Type -> Type -> Label -> (Coercion, Label)
  coerce s1 s2 l | s1 == s2              = (Iden s1, l)                       -- (C-ID)
  coerce (Arr s t) Dyn l                 = (Seq c FuncInj, l')                -- (C-FUN!)                                                  -- (C-B!)
    where (c, l') = coerce (Arr s t) (Arr Dyn Dyn) l 
  coerce (TRef s) Dyn l                  = (Seq c RefInj, l')                 -- (C-REF!)
    where (c, l') = coerce (TRef s) (TRef Dyn) l
  coerce ty Dyn l                        = (Inject ty, l)                     -- (C-B!)
  coerce Dyn (Arr s t) l                 = (Seq (FuncProj l) c, l')           -- (C-FUN?)
    where (c, l') = coerce (Arr Dyn Dyn) (Arr s t) (increment l)
  coerce Dyn (TRef s) l                  = (Seq (RefProj l) c, l')            -- (C-REF?)
    where (c, l') = coerce (TRef Dyn) (TRef s) (increment l)
  coerce Dyn ty l                        = (Project ty l, increment l)        -- (C-B?)
  coerce (Arr s1 s2) (Arr t1 t2) l 
    | Arr s1 s2 `isConsistent` Arr t1 t2 = (Func c d, l2)                     -- (C-FUN)
    where (c, l1) = coerce t1 s1 l
          (d, l2) = coerce s2 t2 l1
  coerce (TRef s) (TRef t) l
    | s `isConsistent` t                 = (CRef c d, l2)                     -- (C-REF)
    where (c, l1) = coerce t s l
          (d, l2) = coerce s t l1
  coerce s1 s2 l                         = (Fail s1 s2 l, increment l)        -- (C-FAIL)

  -- reduce a coercion (single-step)
  reduceCoercion :: Coercion -> Coercion
  reduceCoercion (Seq (Iden _) c)                    = c                           -- (L-ID1)
  reduceCoercion (Seq c (Iden _))                    = c                           -- (L-ID2)
  reduceCoercion (Seq (Fail s1 s2 l) _)              = Fail s1 s2 l                -- (L-FAILL)
  reduceCoercion (Seq (Inject _) (Fail s1 s2 l))     = Fail s1 s2 l                -- (L-FAILR)
  reduceCoercion (Seq (Inject s1) (Project s2 l)) 
    | s1 == s2                                       = Iden s1                     -- (L-INJPROJ)
    | otherwise                                      = Fail s1 s2 l                -- (L-FAIL
  reduceCoercion (Func (Iden ty) (Iden _))           = Iden ty                     -- (L-FUNID)
  reduceCoercion (Func (Fail s1 s2 l) _)             = Fail s1 s2 l                -- (E-FUNFAILL)
  reduceCoercion (Func c (Fail s1 s2 l))
    | isNormalized c                                 = Fail s1 s2 l                -- (E-FUNFAILR)
  reduceCoercion (Func c d)    
    | isNormalized c                                 = Func c $ reduceCoercion d   -- (L-FUN1)
    | otherwise                                      = reduceCoercion c `Func` d   -- (L-FUN2)
  reduceCoercion (CRef (Iden ty) (Iden _))           = Iden ty                     -- (L-FUNID)
  reduceCoercion (CRef (Fail s1 s2 l) _)             = Fail (TRef s1) (TRef s2) l  -- (E-REFFAILL)
  reduceCoercion (CRef c (Fail s1 s2 l))
    | isNormalized c                                 = Fail (TRef s1) (TRef s2) l  -- (E-REFFAILR)
  reduceCoercion (CRef c d)    
    | isNormalized c                                 = CRef c $ reduceCoercion d   -- (L-REF1)
    | otherwise                                      = reduceCoercion c `CRef` d   -- (L-REF2)
  reduceCoercion (Seq (CRef c1 c2) (CRef d1 d2))     
    | all isNormalized [c1, c2, d1, d2]              = Seq d1 c1 `CRef` Seq c2 d2  -- (L-SEQREF)
  reduceCoercion (Seq (Func c1 c2) (Func d1 d2))                          
    | all isNormalized [c1, c2, d1, d2]              = Seq d1 c1 `Func` Seq c2 d2  -- (L-SEQFUN)
  reduceCoercion (Seq (Seq c1 c2) c3)                        
    | isNormalized $ Seq c1 c2                       = Seq c1 $ Seq c2 c3          -- (L-ASS1)
  reduceCoercion (Seq c1 (Seq c2 c3))                       
    | isNormalized $ Seq c2 c3                       = Seq c1 c2 `Seq` c3          -- (L-ASS2)
  reduceCoercion (Seq c d)                                  
    | isNormalized c                                 = Seq c $ reduceCoercion d    -- (L-SEQ1)
    | otherwise                                      = reduceCoercion c `Seq` d    -- (L-SEQ2)
  reduceCoercion c                                   = c
 
  -- normalize a coercion (big-step reduction)
  normalize :: Coercion -> Coercion
  normalize c 
    | isNormalized c = c
    | otherwise      = normalize $ reduceCoercion c

  -- find the source and target type of a coercion
  getCoercionTypes :: Coercion -> (Type, Type) 
  getCoercionTypes (Iden ty)        = (ty, ty)
  getCoercionTypes (Project ty _)   = (Dyn, ty)
  getCoercionTypes (Inject ty)      = (ty, Dyn)
  getCoercionTypes (Func c1 c2)     = (Arr s1 s2, Arr t1 t2)
    where (t1, s1) = getCoercionTypes c1
          (s2, t2) = getCoercionTypes c2
  getCoercionTypes (CRef _ c)       = (TRef s, TRef t)
    where (s, t) = getCoercionTypes c
  getCoercionTypes (Seq c1 c2)      = (s1, t2)
    where (s1, s2) = getCoercionTypes c1
          (t1, t2) = getCoercionTypes c2
  getCoercionTypes (Fail s1 s2 _) = (s1, s2)