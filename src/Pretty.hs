module Pretty (
    printPretty
  ) where
  
  import Syntax
  
  import Text.PrettyPrint (Doc, (<>), (<+>))
  import qualified Text.PrettyPrint as PP
  
  output :: Term -> Doc
  output t = case t of 
    Zero            -> PP.text "0"
    Tru             -> PP.text "true"
    Fls             -> PP.text "false"
    Var _ varName   -> PP.text varName
    Succ Zero       -> PP.text "succ" <+> output Zero
    Succ t          -> PP.text "succ" <+> PP.parens (output t)
    Pred Zero       -> PP.text "pred" <+> output Zero
    Pred t          -> PP.text "pred" <+> PP.parens (output t)
    Lambda _ t ctx  -> PP.text "\\" 
                      <+> PP.text (head ctx) 
                      <+> PP.text "."
                      <+> output t
    App t1 t2       -> PP.parens (output t1) <+> PP.parens (output t2)
  
  printPretty :: Term -> String
  printPretty = PP.render . output