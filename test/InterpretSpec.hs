module InterpretSpec where 

  import Test.Hspec 
  import Types
  import Syntax
  import Parser
  import TypeChecker
  import Evaluator
  import Errors

  {-# ANN module "HLint: ignore Reduce duplication" #-}

  spec :: Spec
  spec = 
    describe "interprets" $ do
      context "(\\x. x:=succ 0) ref 0" $ 
        it "should be succ 0" $ do
          let Right expr    = parseExpr "(\\x. x:=succ 0) ref 0"
          let Right coerced = typeCheck expr 
          let Right res     = evaluate coerced
          res `shouldBe` Succ Zero

      context "(\\m. ((\\x. (x ref (succ (succ 0)))) m)) (\\y:Ref Nat. !y)" $ 
        it "should be succ (succ 0)" $ do 
          let Right expr    = parseExpr "(\\m. ((\\x. (x ref (succ (succ 0)))) m)) (\\y:Ref Nat. !y)"
          let Right coerced = typeCheck expr 
          let Right res     = evaluate coerced
          res `shouldBe` Succ (Succ Zero)

      context "(\\m. ((\\x. (x (succ (succ 0)))) m)) (\\y:Ref Nat. !y)" $ 
        it "should be blame 1" $ do 
          let Right expr    = parseExpr "(\\m. ((\\x. (x (succ (succ 0)))) m)) (\\y:Ref Nat. !y)"
          let Right coerced = typeCheck expr 
          let Right res     = evaluate coerced
          res `shouldBe` Blame (Label 1)

      context "(\\m. ((\\x. (x 0)) m)) (\\y:Nat. succ y)" $ 
        it "should be succ 0" $ do 
          let Right expr    = parseExpr "(\\m. ((\\x. (x 0)) m)) (\\y:Nat. succ y)"
          let Right coerced = typeCheck expr 
          let Right res     = evaluate coerced
          res `shouldBe` Succ Zero

      context "(\\m. ((\\x:Nat->Bool. (x 0)) m)) (\\y:Nat. succ y)" $ 
        it "should be blame 1" $ do 
          let Right expr    = parseExpr "(\\m. ((\\x:Nat->Bool. (x 0)) m)) (\\y:Nat. succ y)"
          let Right coerced = typeCheck expr 
          let Right res     = evaluate coerced
          res `shouldBe` Blame (Label 1)

      context "(\\x. (x 0)) (\\x:Nat. (succ x))" $ 
        it "should be succ 0" $ do 
          let Right expr    = parseExpr "(\\x. (x 0)) (\\x:Nat. (succ x))"
          let Right coerced = typeCheck expr 
          let Right res     = evaluate coerced
          res `shouldBe` Succ Zero

      context "(\\n. (\\m. if (\\x. iszero x) n then m 0 else m (succ 0)) (\\y. succ y)) 0" $ 
        it "should be succ 0" $ do 
          let Right expr    = parseExpr "(\\n. (\\m. if (\\x. iszero x) n then m 0 else m (succ 0)) (\\y. succ y)) 0"
          let Right coerced = typeCheck expr 
          let Right res     = evaluate coerced
          res `shouldBe` Succ Zero

      context "(\\x:Nat->Bool. x 0) (\\x:Nat. x)" $ 
        it "should be Type Mismatch" $ do 
          let Right expr    = parseExpr "(\\x:Nat->Bool. x 0) (\\x:Nat. x)"
          typeCheck expr `shouldBe` Left (Mismatch (Arr Nat Nat) (Arr Nat Bool))

      context "((\\m. if (\\x. iszero x) m then (\\x. succ x) else (\\x. pred x)) 0) true" $ 
        it "should be blame 1" $ do 
          let Right expr    = parseExpr "((\\m. if (\\x. iszero x) m then (\\x. succ x) else (\\x. pred x)) 0) true"
          let Right coerced = typeCheck expr 
          let Right res     = evaluate coerced
          res `shouldBe` Blame (Label 1)

      context "(\\x:Nat->Nat. x (x 0)) (\\x. (succ x))" $ 
        it "should be succ (succ 0)" $ do 
          let Right expr    = parseExpr "(\\x:Nat->Nat. x (x 0)) (\\x. (succ x))"
          let Right coerced = typeCheck expr 
          let Right res     = evaluate coerced
          res `shouldBe` Succ (Succ Zero)

      context "(\\m. ((\\x:Nat->Nat. (x 0)) m)) (\\y:Nat. iszero y)" $ 
        it "should be blame 1" $ do 
          let Right expr    = parseExpr "(\\m. ((\\x:Nat->Nat. (x 0)) m)) (\\y:Nat. iszero y)"
          let Right coerced = typeCheck expr 
          let Right res     = evaluate coerced
          res `shouldBe` Blame (Label 1)