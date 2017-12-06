module Pretty (
    printPretty
  ) where
  
    import Syntax
    import Types

    import Text.PrettyPrint (Doc, (<>), (<+>))
    import qualified Text.PrettyPrint as PP

    -- stringify the specified type
    typeName :: Type -> String
    typeName ty = case ty of 
      Nat          -> "Nat"
      Bool         -> "Bool"
      Arr ty1 ty2  -> typeName ty1 ++ "->" ++ typeName ty2

    output :: Term -> Doc
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
                        <+> PP.text (typeName ty)
                        <+> PP.text "."
                        <+> output t
      App t1 t2        -> PP.parens (output t1) <+> PP.parens (output t2)

    printPretty :: Term -> String
    printPretty = PP.render . output