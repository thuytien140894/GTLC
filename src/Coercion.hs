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
  combine :: Coercion -> Coercion -> Coercion
  combine (Iden _) c                       = c
  combine c (Iden _)                       = c
  combine (Fail ty1 ty2) _                     = Fail ty1 ty2 
  combine _ (Fail ty1 ty2)                     = Fail ty1 ty2
  combine (Inject ty1) (Project ty2) 
    | ty1 == ty2                               = Iden ty1
    | otherwise                                = Fail ty1 ty2
  combine (Func c1 c2) (Func d1 d2)            = Func (combine d1 c1) (combine c2 d2)
  combine c1 c2                                = Seq c1 c2

-- find the source and target type of a coercion
  getTypes :: Coercion -> (Type, Type) 
  getTypes (Iden ty)       = (ty, ty)
  getTypes (Project ty)    = (Dyn, ty)
  getTypes (Inject ty)     = (ty, Dyn)
  getTypes (Func c1 c2)    = (Arr s1 s2, Arr t1 t2)
    where (t1, s1) = getTypes c1
          (s2, t2) = getTypes c2
  getTypes (Seq c1 c2)     = (s1, t2)
    where (s1, s2) = getTypes c1
          (t1, t2) = getTypes c2
  getTypes (Fail ty1 ty2)    = (ty1, ty2)
                                    





 



