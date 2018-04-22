module Prettier (
  printPretty
  ) where
  
  import Syntax
  import Types
  import Errors

  import Text.PrettyPrint (Doc, (<>), (<+>), ($$))
  import qualified Text.PrettyPrint as PP

  -- format the list of entries in a record
  outputRcdEntries :: [(String, Term)] -> Doc
  outputRcdEntries []               = PP.empty
  outputRcdEntries ((l1, t1) : ys)  = PP.text l1 <> PP.equals <> output t1 <> optionalComma
                                      where optionalComma = case ys of 
                                                              []    -> PP.empty
                                                              _     -> PP.comma <> outputRcdEntries ys
  
  -- format the list of entry types in a record                                                           
  outputRcdTypes :: [(String, Type)] -> Doc
  outputRcdTypes []                 = PP.empty
  outputRcdTypes ((l1, ty1) : ys)   = PP.text l1 <> PP.colon <> output ty1 <> optionalComma
                                      where optionalComma = case ys of 
                                                              []    -> PP.empty
                                                              _     -> PP.comma <> outputRcdTypes ys
                                                               
  -- type class for pretty printing
  class Pretty a where 
    output :: a -> Doc

    printPretty :: a -> String
    printPretty = PP.render . output

  -- pretty printing for term
  instance Pretty Term where 
    output t = case t of 
      Zero                 -> PP.text "0"
      Tru                  -> PP.text "true"
      Fls                  -> PP.text "false"
      Var _ _ varName      -> PP.text varName
      Succ Zero            -> PP.text "succ" <+> output Zero
      Succ t'              -> PP.text "succ" <+> PP.parens (output t')
      Pred Zero            -> PP.text "pred" <+> output Zero
      Pred t'              -> PP.text "pred" <+> PP.parens (output t') 
      IsZero Zero          -> PP.text "iszero" <+> output Zero
      IsZero t'            -> PP.text "iszero" <+> PP.parens (output t') 
      If t1 t2 t3          -> PP.text "if" 
                              <+> output t1  
                              <+> PP.text "then"
                              <+> output t2
                              <+> PP.text "else"
                              <+> output t3
      Rec ls               -> PP.braces (outputRcdEntries ls)
      Proj rcd l           -> output rcd <> PP.text "." <> PP.text l
      Ref t'               -> PP.text "*" <> PP.parens (output t')
      Deref t'             -> PP.text "!" <> PP.parens (output t')
      Assign t1 t2         -> output t1 <+> PP.text ":=" <+> output t2
      Loc l                -> PP.text "0x" <> PP.int l
      Lambda ty t' ctx     -> PP.text "\\" 
                              <+> PP.text (head ctx)
                              <+> PP.colon
                              <+> output ty
                              <+> PP.text "."
                              <+> output t'
      App t1 t2            -> case t1 of 
                                Lambda {} -> PP.parens (output t1) <+> sndTerm
                                App _ _   -> PP.parens (output t1) <+> sndTerm
                                _         -> output t1 <+> sndTerm
                                where sndTerm = case t2 of 
                                                  Lambda{}  -> PP.parens $ output t2
                                                  App _ _   -> PP.parens $ output t2
                                                  _         -> output t2

  -- pretty printing for type                                            
  instance Pretty Type where 
    output ty = case ty of 
      Top                 -> PP.text "Top"
      Nat                 -> PP.text "Nat"
      Bool                -> PP.text "Bool"
      TRef ty'            -> PP.text "Ref" <+> output ty'
      Arr ty1 ty2         -> output ty1 <> PP.text "->" <> output ty2
      TRec ls             -> PP.braces $ outputRcdTypes ls

  -- pretty printing for type error
  instance Pretty TypeError where 
    output e = case e of 
      NotBound t                     -> PP.text "Variable not bound:" 
                                        <+> output t
      NotBool ty                     -> PP.text "Conditional expects boolean condition, but got:"
                                        <+> output ty
      NotNat ty                      -> PP.text "Numeric expression is expected, but got:"
                                        <+> output ty
      Difference ty1 ty2             -> PP.text "Type difference for conditional branches:" 
                                        <+> output ty1 
                                        <+> PP.text "vs" 
                                        <+> output ty2 
      Mismatch actualTy expectedTy   -> PP.text "Type mismatch for function argument" 
                                        $$ PP.nest 4 (PP.text "got:" <+> output actualTy)
                                        $$ PP.nest 4 (PP.text "but expected:" <+> output expectedTy)
      NotFunction t                  -> PP.text "Couldn't apply to non-function expression:" 
                                        <+> output t
      IllegalAssign t                -> PP.text "Couldn't assign to non-reference" 
                                        <+> output t
      IllegalDeref t                 -> PP.text "Couldn't dereference non-reference"
                                        <+> output t
      NotRecord t                    -> PP.text "Couldn't perform projection on non-record expression:" 
                                        <+> output t
      InvalidLabel l                 -> PP.text "Non-existent label on record:" 
                                        <+> PP.text l

  instance Pretty RuntimeError where 
    output e = case e of 
      InvalidRef l               -> PP.text "Non-existent reference at location" <+> PP.int l
      CastError srcTy cstTy      -> PP.text "Illegal cast from" 
                                    <+> output srcTy 
                                    <+> PP.text "to" 
                                    <+> output cstTy         