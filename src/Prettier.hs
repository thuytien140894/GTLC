module Prettier (
    printPretty
  ) where
  
    import Syntax
    import Types
    import TypeErrors

    import Text.PrettyPrint (Doc, (<>), (<+>))
    import qualified Text.PrettyPrint as PP

    -- type class for pretty printing
    class Pretty a where 
      output :: a -> Doc

      printPretty :: a -> String
      printPretty = PP.render . output

    -- pretty printing for term
    instance Pretty Term where 
      output t = case t of 
        Zero             -> PP.text "0"
        Tru              -> PP.text "true"
        Fls              -> PP.text "false"
        Var _ _ varName  -> PP.text varName
        Succ Zero        -> PP.text "succ" <+> output Zero
        Succ t           -> PP.text "succ" <+> PP.parens (output t)
        Pred Zero        -> PP.text "pred" <+> output Zero
        Pred t           -> PP.text "pred" <+> PP.parens (output t)
        Lambda ty t ctx  -> PP.text "\\" 
                          <+> PP.text (head ctx)
                          <+> PP.text ":"
                          <+> output ty
                          <+> PP.text "."
                          <+> output t
        App t1 t2        -> case t1 of 
                              Var {} -> output t1 <+> sndTerm
                              _         -> PP.parens (output t1) <+> sndTerm
                              where sndTerm = case t2 of 
                                                Var {} -> output t2
                                                _         -> PP.parens (output t2)

    -- pretty printing for type                                            
    instance Pretty Type where 
      output ty = case ty of 
        Nat              -> PP.text "Nat"
        Bool             -> PP.text "Bool"
        Arr ty1 ty2      -> output ty1 <> PP.text "->" <> output ty2

    -- pretty printing for type error
    instance Pretty TypeError where 
      output e = case e of 
        NotBound t       -> output t <+> PP.text "does not have a bound type."
        Difference t1 t2 -> output t1 
                          <+> PP.text "and"
                          <+> output t2 
                          <+> PP.text "do not have the same type."
        Mismatch t ty    -> output t 
                          <+> PP.text "does not have the expected type"
                          <+> output ty
        NotFunction t    -> output t <+> PP.text "is not of type function." 
        IllTyped         -> PP.text "Ill-Typed"