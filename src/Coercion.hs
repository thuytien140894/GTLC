module Coercion where

  import Syntax
  import Types

  -- coercion type system
  coerce :: Type -> Type -> Coercion
  coerce ty Dyn                  = case ty of                     
    Arr _ _     -> coerce ty (Arr Dyn Dyn)                            -- (C-FUN!)
    _           -> Inject ty                                          -- (C-B!)
  coerce Dyn ty                  = case ty of 
    Arr _ _     -> coerce (Arr Dyn Dyn) ty                            -- (C-FUN?)
    _           -> Project ty                                         -- (C-B?)
  coerce (Arr s1 s2) (Arr t1 t2) = Func (coerce t1 s1) (coerce s2 t2) -- (C-FUN)
  coerce ty1 ty2 | ty1 == ty2    = Iden ty1                           -- (C-ID)
                 | otherwise     = Fail ty1 ty2                       -- (C-FAIL)

  -- reduction rules
  combineCoercions :: Coercion -> Coercion -> Coercion
  combineCoercions (Iden _) c                           = c
  combineCoercions c (Iden _)                           = c
  combineCoercions (Fail ty1 ty2) _                     = Fail ty1 ty2 
  combineCoercions _ (Fail ty1 ty2)                     = Fail ty1 ty2
  combineCoercions (Inject ty1) (Project ty2) 
    | ty1 == ty2                                        = Iden ty1
    | otherwise                                         = Fail ty1 ty2
  combineCoercions (Func c1 c2) (Func d1 d2)            = Func (combineCoercions d1 c1) (combineCoercions c2 d2)
  combineCoercions c1 c2                                = Seq c1 c2

  -- find the source and target type of a coercion
  getCoercionTypes :: Coercion -> (Type, Type) 
  getCoercionTypes (Iden ty)       = (ty, ty)
  getCoercionTypes (Project ty)    = (Dyn, ty)
  getCoercionTypes (Inject ty)     = (ty, Dyn)
  getCoercionTypes (Func c1 c2)    = (Arr s1 s2, Arr t1 t2)
    where (t1, s1) = getCoercionTypes c1
          (s2, t2) = getCoercionTypes c2
  getCoercionTypes (Seq c1 c2)     = (s1, t2)
    where (s1, s2) = getCoercionTypes c1
          (t1, t2) = getCoercionTypes c2
  getCoercionTypes (Fail ty1 ty2)    = (ty1, ty2)
                                    





 



