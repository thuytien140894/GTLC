module Coercion where 

  import Syntax
  import Types

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

  -- reduction rules
  combineCoercions :: Coercion -> Coercion -> Coercion
  combineCoercions (Iden _) c                           = c
  combineCoercions c (Iden _)                           = c
  combineCoercions (Fail ty1 ty2 l) _                   = Fail ty1 ty2 l
  combineCoercions _ (Fail ty1 ty2 l)                   = Fail ty1 ty2 l
  combineCoercions (Inject ty1) (Project ty2 l) 
    | ty1 == ty2                                        = Iden ty1
    | otherwise                                         = Fail ty1 ty2 l
  combineCoercions (Func c1 c2) (Func d1 d2)            = Func (combineCoercions d1 c1) (combineCoercions c2 d2)
  combineCoercions c1 c2                                = Seq c1 c2

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
                                    





 



