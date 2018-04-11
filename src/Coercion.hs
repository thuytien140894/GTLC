module Coercion where 

  import Syntax
  import Types

  -- increment the label index
  incrementLabel :: Label -> Label
  incrementLabel (Label l) = Label $ l + 1

  -- consistency rules
  isConsistent :: Type -> Type -> Bool
  isConsistent Dyn _                               = True
  isConsistent _ Dyn                               = True
  isConsistent ty1 ty2 | ty1 == ty2                = True
  isConsistent (Arr s1 s2) (Arr t1 t2) 
    | isConsistent t1 s1 &&
      isConsistent s2 t2                           = True
  isConsistent _ _                                 = False

  -- check if a coercion is normalized (cannot be further reduced)
  isNormalized :: Coercion -> Bool
  isNormalized (Inject _)                         = True
  isNormalized (Project _ _)                      = True
  isNormalized (Iden _)                           = True
  isNormalized (Func (Iden _) (Iden _))           = False
  isNormalized (Func c d) 
    | isNormalized c && isNormalized d            = True
  isNormalized (Seq c (Inject _))                 
    | isNormalized c                              = True
  isNormalized (Seq (Project _ _) c)              
    | isNormalized c                              = True
  isNormalized (Seq (Project _ _) Fail{})         = True
  isNormalized (Seq (Func c d) Fail{}) 
    | isNormalized c && isNormalized d            = True
  isNormalized (Seq c FuncInj) 
    | isNormalized c                              = True
  isNormalized (Seq (FuncProj _) c)
    | isNormalized c                              = True
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
  coerce ty Dyn l                        = case ty of                     
    Arr _ _     -> (Seq c FuncInj, l')                                             -- (C-FUN!)
    _           -> (Inject ty, l)                                                  -- (C-B!)
    where (c, l') = coerce ty (Arr Dyn Dyn) l 
  coerce Dyn ty l                        = case ty of 
    Arr _ _     -> (Seq (FuncProj l) c, l')                                        -- (C-FUN?)
    _           -> (Project ty l, incrementLabel l)                                -- (C-B?)
    where (c, l') = coerce (Arr Dyn Dyn) ty (incrementLabel l)
  coerce (Arr s1 s2) (Arr t1 t2) l 
    | Arr s1 s2 `isConsistent` Arr t1 t2 = (Func c d, l2)                          -- (C-FUN)
    where (c, l1) = coerce t1 s1 l
          (d, l2) = coerce s2 t2 l1
  coerce ty1 ty2 l | ty1 == ty2          = (Iden ty1, l)                           -- (C-ID)
  coerce ty1 ty2 l                       = (Fail ty1 ty2 l, incrementLabel l)      -- (C-FAIL)

  -- reduce a coercion (single-step)
  reduceCoercion :: Coercion -> Coercion
  reduceCoercion (Seq (Iden _) c)                    = c                           -- (L-ID1)
  reduceCoercion (Seq c (Iden _))                    = c                           -- (L-ID2)
  reduceCoercion (Seq (Fail ty1 ty2 l) _)            = Fail ty1 ty2 l              -- (L-FAILL)
  reduceCoercion (Seq (Inject _) (Fail ty1 ty2 l))   = Fail ty1 ty2 l              -- (L-FAILR)
  reduceCoercion (Seq (Inject ty1) (Project ty2 l)) 
    | ty1 == ty2                                     = Iden ty1                    -- (L-INJPROJ)
    | otherwise                                      = Fail ty1 ty2 l              -- (L-FAIL)
  reduceCoercion (Seq (Func c1 c2) (Func d1 d2))                          
    | all isNormalized [c1, c2, d1, d2]              = Seq d1 c1 `Func` Seq c2 d2  -- (L-SEQFUN)
  reduceCoercion (Seq (Seq c1 c2) c3)                        
    | isNormalized $ Seq c1 c2                       = Seq c1 $ Seq c2 c3          -- (L-ASS1)
  reduceCoercion (Seq c1 (Seq c2 c3))                       
    | isNormalized $ Seq c2 c3                       = Seq c1 c2 `Seq` c3          -- (L-ASS2)
  reduceCoercion (Seq c d)                                  
    | isNormalized c                                 = Seq c $ reduceCoercion d    -- (L-SEQ1)
    | otherwise                                      = reduceCoercion c `Seq` d    -- (L-SEQ2)
  reduceCoercion (Func (Iden ty) (Iden _))           = Iden ty                     -- (L-FUNID)
  reduceCoercion (Func (Fail ty1 ty2 l) _)           = Fail ty1 ty2 l              -- (E-FUNFAILL)
  reduceCoercion (Func c (Fail ty1 ty2 l))
    | isNormalized c                                 = Fail ty1 ty2 l              -- (E-FUNFAILR)
  reduceCoercion (Func c d)    
    | isNormalized c                                 = Func c $ reduceCoercion d   -- (L-FUN1)
    | otherwise                                      = reduceCoercion c `Func` d   -- (L-FUN2)
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
  getCoercionTypes (Seq c1 c2)      = (s1, t2)
    where (s1, s2) = getCoercionTypes c1
          (t1, t2) = getCoercionTypes c2
  getCoercionTypes (Fail ty1 ty2 _) = (ty1, ty2)