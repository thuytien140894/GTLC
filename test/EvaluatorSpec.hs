module EvaluatorSpec where

    import Test.Hspec
    import Syntax
    import Evaluator
    import Types
    import Parser
    import Errors
    import TypeChecker
    import Subtype

    spec :: Spec
    spec = 
      describe "evaluates" $ do
        context "(\\x:Nat->Nat. x (succ 0)) (\\y:Nat. y)" $
          it "should be succ 0" $
            evaluate (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (Succ Zero)) ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"])) `shouldBe` 
            Just (Succ Zero)
        
        context "iszero (pred (succ 0))" $
          it "should be true" $
            evaluate (IsZero (Pred (Succ Zero))) `shouldBe` Just Tru

        context "{x=if true then (iszero 0) else false}" $ 
          it "should be {x=true}" $ 
            evaluate (Rec [("x", If Tru (IsZero Zero) Fls)]) `shouldBe` Just (Rec [("x",Tru)])

        context "{x=if true then (iszero 0) else false}.x" $ 
          it "should be true" $ 
            evaluate (Proj (Rec [("x", If Tru (IsZero Zero) Fls)]) "x") `shouldBe` Just Tru

        context "(\\x:Nat->Nat. (x 0)) (\\x:Nat. (succ x))" $ 
          it "should be succ 0" $ 
            evaluate (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Lambda Nat (Succ (Var 0 Nat "x")) ["x"])) `shouldBe` 
            Just (Succ Zero)

        context "(\\x:Nat->Bool. (x (succ 0))) (\\x:Nat. (iszero x))" $ 
          it "should be false" $ 
            evaluate (App (Lambda (Arr Nat Bool) (App (Var 0 (Arr Nat Bool) "x") (Succ Zero)) ["x"]) (Lambda Nat (IsZero (Var 0 Nat "x")) ["x"])) `shouldBe` 
            Just Fls

        context "(\\x. (x <Nat!>0)) <Nat?->Nat!>(\\x:Nat. (succ x))" $
          it "should be succ 0" $
            evaluate (App (Lambda Dyn (App (Var 0 Dyn "x") (Cast (Inject Nat) Zero)) ["x"]) (Cast (Func (Project Nat) (Inject Nat)) (Lambda Nat (Succ (Var 0 Nat "x")) ["x"])))
            `shouldBe` Just (Succ Zero)