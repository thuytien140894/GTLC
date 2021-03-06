module Prettier 
    ( printExp
    , printMsg 
    , printRes
    ) where
    
    import Error
    import Syntax
    import Type
    import Utils (getFailedTerm, removeCasts, isSingular)

    import Data.Maybe (fromJust)
    import Text.PrettyPrint.ANSI.Leijen (Doc, (<>), (<+>), (<$$>))
    
    import qualified Text.PrettyPrint.ANSI.Leijen as PP

    -- | Format the list of entries in a record.
    outputRcdEntries :: [(String, Term)] -> Doc
    outputRcdEntries []              = PP.empty
    outputRcdEntries ((l1, t1) : ys) = PP.text l1 
                                       <> PP.equals 
                                       <> output t1
                                       <> optionalComma
      where 
        optionalComma = case ys of 
            [] -> PP.empty
            _  -> PP.comma <> outputRcdEntries ys
    
    -- | Format the list of entry types in a record.                                                           
    outputRcdTypes :: [(String, Type)] -> Doc
    outputRcdTypes []              = PP.empty
    outputRcdTypes ((l1, s1) : ys) = PP.text l1 
                                     <> PP.colon 
                                     <> output s1 
                                     <> optionalComma
      where 
        optionalComma = case ys of 
            [] -> PP.empty
            _  -> PP.comma <> outputRcdTypes ys

    -- | Format an exception message.
    renderException :: String -> Doc
    renderException = PP.red . PP.text

    -- | Type class to print pretty.
    class Pretty a where 
        output :: a -> Doc

        -- | Print error messages.
        printMsg :: a -> IO ()
        printMsg = PP.putDoc . output

        -- | Print a valid result.
        printRes :: a -> IO ()
        printRes a = PP.putDoc (PP.text "==>" <+> output a)

        -- | Print a cast expression.
        printExp :: a -> IO () 
        printExp a = PP.putDoc $ PP.green (PP.text "TypeChecked:") 
                     <+> output a <> PP.linebreak

    -- | Print pretty for terms.
    instance Pretty Term where 
        output t = case t of 
            Zero                -> PP.text "0"
            Tru                 -> PP.text "true"
            Fls                 -> PP.text "false"
            Var _ _ varName     -> PP.text varName
            Succ t' 
                | isSingular t' -> PP.text "succ" <+> output t'
            Succ t'             -> PP.text "succ" <+> PP.parens (output t')
            Pred t' 
                | isSingular t' -> PP.text "pred" <+> output t'
            Pred t'             -> PP.text "pred" <+> PP.parens (output t') 
            IsZero t' 
                | isSingular t' -> PP.text "iszero" <+> output t'
            IsZero t'           -> PP.text "iszero" <+> PP.parens (output t') 
            If t1 t2 t3         -> PP.text "if" 
                                   <+> output t1  
                                   <+> PP.text "then"
                                   <+> output t2
                                   <+> PP.text "else"
                                   <+> output t3
            Rec ls              -> PP.braces (outputRcdEntries ls)
            Proj rcd l          -> output rcd <> PP.text "." <> PP.text l
            Ref t' 
                | isSingular t' -> PP.text "ref" <+> output t'
            Ref t'              -> PP.text "ref" <> PP.parens (output t')
            Deref t' 
                | isSingular t' -> PP.text "!" <> output t'
            Deref t'            -> PP.text "!" <> PP.parens (output t')
            Assign t1 t2        -> output t1 <+> PP.text ":=" <+> output t2
            Loc l               -> PP.text "0x" <> PP.int l
            Cast c t' 
                | isSingular t' -> PP.angles (output c) <> output t'
            Cast c t'           -> PP.angles (output c) <> PP.parens (output t')
            Lambda ty t' ctx    -> PP.backslash
                                   <> PP.text (head ctx)
                                   <> PP.colon
                                   <> output ty
                                   <> PP.text "."
                                   <+> output t'
            App t1 t2           -> case t1 of 
                                       Lambda {} -> PP.parens (output t1) <+> sndTerm
                                       App _ _   -> PP.parens (output t1) <+> sndTerm
                                       _         -> output t1 <+> sndTerm
                                     where 
                                       sndTerm = case t2 of 
                                           Lambda{} -> PP.parens $ output t2
                                           App _ _  -> PP.parens $ output t2
                                           _        -> output t2

    -- | Print pretty for coercions.
    instance Pretty Coercion where 
        output c = case c of 
            Iden ty      -> PP.text "I"
            FuncProj _   -> PP.text "Fun?"
            RefProj _    -> PP.text "Ref?"
            Project ty _ -> output ty <> PP.text "?"
            FuncInj      -> PP.text "Fun!"
            Fail{}       -> PP.text "CastError"
            RefInj       -> PP.text "Ref!"
            Inject ty    -> output ty <> PP.text "!"
            CRef c1 c2   -> PP.text "Ref" <+> output c1 <+> output c2
            Func c1 c2   -> output c1 <> PP.text "->" <> output c2
            Seq c1 c2    -> output c1 <> PP.semi <> output c2

    -- | Print pretty for types.
    instance Pretty Type where 
        output ty = case ty of 
            Top       -> PP.text "Top"
            Dyn       -> PP.text "?"
            Nat       -> PP.text "Nat"
            Boolean   -> PP.text "Bool"
            TRef ty'  -> PP.text "Ref" <+> output ty'
            Arr s1 s2 -> output s1 <> PP.text "->" <> output s2
            TRec ls   -> PP.braces $ outputRcdTypes ls

    -- | Print pretty for type errors.
    instance Pretty TypeError where 
        output e = renderException "Type error:" <+> case e of 
            NotBound t             -> PP.text "Variable not in scope:" 
                                      <+> PP.squotes (output t)
            NotBool ty             -> PP.text "Conditional expects boolean condition, but got:"
                                      <+> output ty
            NotNat ty              -> PP.text "Numeric expression is expected, but got:"
                                      <+> output ty
            Difference s1 s2       -> PP.text "Type difference for conditional branches:" 
                                      <+> output s1 
                                      <+> PP.text "vs" 
                                      <+> output s2 
            FunMismatch s1 s2 t    -> PP.text "Type mismatch for function application:" 
                                      <+> PP.squotes (output t) 
                                      <$$> PP.indent 4 (PP.text "got:" <+> output s1)
                                      <$$> PP.indent 4 (PP.text "but expected:" <+> output s2)
            NotFunction t          -> PP.text "Cannot apply to non-function expression:" 
                                      <+> PP.squotes (output t)
            IllegalAssign t        -> PP.text "Cannot assign to non-reference expression:" 
                                      <+> PP.squotes (output t)
            AssignMismatch s1 s2 t -> PP.text "Type mismatch for assignment:" 
                                      <+> PP.squotes (output t) 
                                      <$$> PP.indent 4 (PP.text "got:" <+> output s1)
                                      <$$> PP.indent 4 (PP.text "but expected:" <+> output s2)
            IllegalDeref t         -> PP.text "Cannot dereference non-reference expression:"
                                      <+> PP.squotes (output t)

    -- | Print pretty for blame results.
    instance Pretty BlameRes where 
        output (BlameRes c t) = case c of 
            FunArg    -> PP.text "In function argument for expression"
                         <+> blamed <+> context
            FunRet    -> PP.text "In function return value for expression"
                         <+> blamed <+> context
            RefRead   -> PP.text "In reference read for expression"
                         <+> blamed <+> context
            RefWrite  -> PP.text "In reference write for expression"
                         <+> blamed <+> context
            Function  -> PP.text "Converting expression" 
                         <+> blamed
                         <+> PP.text "to a function type" <+> context
            Reference -> PP.text "Converting expression" 
                         <+> blamed
                         <+> PP.text "to a reference type" <+> context
            None      -> PP.text "For expression"
                         <+> blamed <+> context
          where 
            t'      = fromJust $ getFailedTerm t
            blamed  = PP.squotes (output t') 
            context = PP.linebreak <> PP.text "When evaluating" 
                      <+> PP.squotes (output $ removeCasts t)

    -- | Print pretty for runtime errors.
    instance Pretty RuntimeError where 
        output e = case e of 
            InvalidRef l        -> renderException "Exception:" 
                                   <+> PP.text "Non-existent reference at location" 
                                   <+> PP.int l
            CastError s1 s2 res -> renderException "Invalid cast exception:"
                                   <+> PP.text "Unable to cast expression of type" 
                                   <+> PP.squotes (output s1) 
                                   <+> PP.text "to type" 
                                   <+> PP.squotes (output s2)
                                   <$$> PP.indent 4 (output res)
            Stuck               -> renderException "Exception:" <+> PP.text "Evaluation is stuck"         