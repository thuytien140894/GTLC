module Coercion where 

  import Syntax
  import Types

  -- check if a coercion is normalized (cannot be further reduced)
  isNormalized :: Coercion -> Bool
  isNormalized (Inject _)                         = True
  isNormalized (Project _ _)                      = True
  isNormalized (Iden _)                           = True
  isNormalized (Func c d) 
    | isNormalized c && isNormalized d            = True
  isNormalized (Seq c (Inject _))                 
    | isNormalized c                              = True
  isNormalized (Seq (Project _ _) c)              
    | isNormalized c                              = True
  isNormalized (Seq (Project _ _) Fail{})         = True
  isNormalized (Seq (Func c d) Fail{}) 
    | isNormalized c && isNormalized d            = True
  isNormalized _                                  = False

  -- check if a coercion is regular (normalized and not Identity or Fail)
  isRegular :: Coercion -> Bool
  isRegular c 
    | isNormalized c && not (isIdentity c)       = True
  isRegular _                                    = False

  -- check if a coercion is a failure
  isFailure :: Coercion -> Bool
  isFailure Fail {}   = True
  isFailure _         = False

  -- check if a coercion is an identity
  isIdentity :: Coercion -> Bool
  isIdentity (Iden _) = True
  isIdentity _        = False

  -- coercion type system
  coerce :: Type -> Type -> LabelIndex -> (Coercion, LabelIndex)
  coerce ty Dyn l                  = case ty of                     
    Arr _ _     -> coerce ty (Arr Dyn Dyn) l                      -- (C-FUN!)
    _           -> (Inject ty, l)                                 -- (C-B!)
  coerce Dyn ty l                  = case ty of 
    Arr _ _     -> coerce (Arr Dyn Dyn) ty l                      -- (C-FUN?)
    _           -> (Project ty l, l + 1)                          -- (C-B?)
  coerce (Arr s1 s2) (Arr t1 t2) l = (Func c d, l2)               -- (C-FUN)
    where (c, l1) = coerce t1 s1 l
          (d, l2) = coerce s2 t2 l1
  coerce ty1 ty2 l | ty1 == ty2    = (Iden ty1, l)                -- (C-ID)
                   | otherwise     = (Fail ty1 ty2 l, l + 1)      -- (C-FAIL)

  -- reduce a coercion (single-step)
  reduceCoercion :: Coercion -> Coercion
  reduceCoercion (Seq (Iden _) c)                         = c
  reduceCoercion (Seq c (Iden _))                         = c
  reduceCoercion (Seq (Fail ty1 ty2 l) _)                 = Fail ty1 ty2 l
  reduceCoercion (Seq (Inject _) (Fail ty1 ty2 l))        = Fail ty1 ty2 l
  reduceCoercion (Seq (Inject ty1) (Project ty2 l)) 
    | ty1 == ty2                                          = Iden ty1
    | otherwise                                           = Fail ty1 ty2 l
  reduceCoercion (Seq (Func c1 c2) (Func d1 d2))            
    | all isNormalized [c1, c2, d1, d2]                   = Func (Seq d1 c1) (Seq c2 d2)
  reduceCoercion (Seq (Seq c1 c2) c3)                       
    | isNormalized c                                      = Seq c1 (Seq c2 c3)
    | otherwise                                           = Seq (reduceCoercion c) c3
    where c = Seq c1 c2
  reduceCoercion (Seq c1 (Seq c2 c3))                       
    | isNormalized c                                      = Seq (Seq c1 c2) c3
    | otherwise                                           = Seq c1 (reduceCoercion c)
    where c = Seq c2 c3
  reduceCoercion (Seq c d)                                  
    | isNormalized c                                      = Seq c (reduceCoercion d)
    | otherwise                                           = Seq (reduceCoercion c) d
  reduceCoercion (Func (Fail ty1 ty2 l) _)                = Fail ty1 ty2 l
  reduceCoercion (Func c (Fail ty1 ty2 l))
    | isNormalized c                                      = Fail ty1 ty2 l
  reduceCoercion (Func c d)    
    | isNormalized c                                      = Func c (reduceCoercion d)
    | otherwise                                           = Func (reduceCoercion c) d
  reduceCoercion c                                        = c
 
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