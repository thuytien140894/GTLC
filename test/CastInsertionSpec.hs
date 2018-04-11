module CastInsertionSpec where

  import Test.Hspec
  import TypeChecker
  import Parser
  import Errors
  import Types
  import Syntax

  spec :: Spec
  spec = 
    describe "casts" $ do
      context "(\\x. 0) True" $
        it "should be (\\x. 0) <Bool!>True" $ 
          typeCheck (App (Lambda Dyn Zero ["x"]) Tru) `shouldBe` 
          Right (App (Lambda Dyn Zero ["x"]) (Cast (Inject Bool) Tru))

      context "(\\x. (x 0)) (\\x:Nat. (succ x))" $
        it "should be (\\x. (<Func?>x <Nat!>0)) <Func!><Nat?->Nat!>(\\x:Nat. (succ x))" $ 
          typeCheck (App (Lambda Dyn (App (Var 0 Dyn "x") Zero) ["x"]) (Lambda Nat (Succ (Var 0 Nat "x")) ["x"])) `shouldBe` 
          Right (App (Lambda Dyn (App (Cast (Project (Arr Dyn Dyn) (Label 0)) (Var 0 Dyn "x")) (Cast (Inject Nat) Zero)) ["x"]) (Cast (Seq (Func (Project Nat (Label 1)) (Inject Nat)) (Inject (Arr Dyn Dyn))) (Lambda Nat (Succ (Var 0 Nat "x")) ["x"])))
          
      context "(\\x:Dyn->Nat. (x 0)) (\\x:Nat. x)" $
        it "should be (\\x:Dyn->Nat. (x <Nat!>0)) <Nat?->Identity>(\\x:Nat. x)" $ 
          typeCheck (App (Lambda (Arr Dyn Nat) (App (Var 0 (Arr Dyn Nat) "x") Zero) ["x"]) (Lambda Nat (Var 0 Nat "x") ["x"])) `shouldBe` 
          Right (App (Lambda (Arr Dyn Nat) (App (Var 0 (Arr Dyn Nat) "x") (Cast (Inject Nat) Zero)) ["x"]) (Cast (Func (Project Nat (Label 0)) (Iden Nat)) (Lambda Nat (Var 0 Nat "x") ["x"])))

      context "(\\x. succ x) true" $
        it "should be (\\x. succ <Nat?>x) <Bool!>true" $
          typeCheck (App (Lambda Dyn (Succ (Var 0 Dyn "x")) ["x"]) Tru) `shouldBe` 
          Right (App (Lambda Dyn (Succ (Cast (Project Nat (Label 0)) (Var 0 Dyn "x"))) ["x"]) (Cast (Inject Bool) Tru))

      context "(\\x:Bool->Nat. succ 0) (\\x:Nat. x)" $
        it "should be (\\x:Bool->Nat. succ <Identity>0) <Fail>(\\x:Nat. x)" $
          typeCheck (App (Lambda (Arr Bool Nat) (Succ Zero) ["x"]) (Lambda Nat (Var 0 Nat "x") ["x"])) `shouldBe` 
          Left (Mismatch (Arr Nat Nat) (Arr Bool Nat))

      context "(\\x:Bool. x) 0" $
        it "should be Mismatch" $
          typeCheck (App (Lambda Bool (Var 0 Bool "x") ["x"]) Zero)
          `shouldBe` Left (Mismatch Nat Bool)

      context "(\\m. ((\\x:Nat->Nat. (x 0)) m)) (\\y:Nat. succ y)" $
        it "should be (\\m. ((\\x:Nat->Nat. (x 0)) <Nat!->Nat?><Func?>m)) <Func!><Nat?->Nat!>(\\y:Nat. succ y)" $
          typeCheck (App (Lambda Dyn (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Var 0 Dyn "m")) ["m","x"]) (Lambda Nat (Succ (Var 0 Nat "y")) ["y"]))
          `shouldBe` Right (App (Lambda Dyn (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Cast (Seq (Project (Arr Dyn Dyn) (Label 0)) (Func (Inject Nat) (Project Nat (Label 1)))) (Var 0 Dyn "m"))) ["m","x"]) (Cast (Seq (Func (Project Nat (Label 2)) (Inject Nat)) (Inject (Arr Dyn Dyn))) (Lambda Nat (Succ (Var 0 Nat "y")) ["y"])))

      context "(\\m. ((\\x:Nat->Nat. (x 0)) m)) (\\y:Nat. iszero y)" $
        it "should be (\\m. ((\\x:Nat->Nat. (x 0)) <Nat!->Nat?><Func?>m)) <Func!><Nat?->Bool!>(\\y:Nat. iszero y)" $
          typeCheck (App (Lambda Dyn (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Var 0 Dyn "m")) ["m","x"]) (Lambda Nat (IsZero (Var 0 Nat "y")) ["y"]))
          `shouldBe` Right (App (Lambda Dyn (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Cast (Seq (Project (Arr Dyn Dyn) (Label 0)) (Func (Inject Nat) (Project Nat (Label 1)))) (Var 0 Dyn "m"))) ["m","x"]) (Cast (Seq (Func (Project Nat (Label 2)) (Inject Bool)) (Inject (Arr Dyn Dyn))) (Lambda Nat (IsZero (Var 0 Nat "y")) ["y"])))

      context "(\\m. ((\\x:Nat->Nat. (x 0)) m)) true" $
        it "should be (\\m. ((\\x:Nat->Nat. (x 0)) <Nat!->Nat?><Func?>m)) <Bool!>true" $
          typeCheck (App (Lambda Dyn (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Var 0 Dyn "m")) ["m","x"]) Tru)
          `shouldBe` Right (App (Lambda Dyn (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Cast (Seq (Project (Arr Dyn Dyn) (Label 0)) (Func (Inject Nat) (Project Nat (Label 1)))) (Var 0 Dyn "m"))) ["m","x"]) (Cast (Inject Bool) Tru))
      
      context "(\\m. ((\\x. (x 0)) m)) (\\y:Nat. succ y)" $
        it "should be (\\m. ((\\x. (<Func?>x <Nat!>0)) m)) <Func!><Nat?->Nat!>(\\y:Nat. succ y)" $
          typeCheck (App (Lambda Dyn (App (Lambda Dyn (App (Var 0 Dyn "x") Zero) ["x"]) (Var 0 Dyn "m")) ["m","x"]) (Lambda Nat (Succ (Var 0 Nat "y")) ["y"]))
          `shouldBe` Right (App (Lambda Dyn (App (Lambda Dyn (App (Cast (Project (Arr Dyn Dyn) (Label 0)) (Var 0 Dyn "x")) (Cast (Inject Nat) Zero)) ["x"]) (Var 0 Dyn "m")) ["m","x"]) (Cast (Seq (Func (Project Nat (Label 1)) (Inject Nat)) (Inject (Arr Dyn Dyn))) (Lambda Nat (Succ (Var 0 Nat "y")) ["y"])))

      context "(\\x:Nat->Nat. x (x 0)) (\\x. (succ x))" $
        it "should be (\\x:Nat->Nat. x (x 0)) <Nat!->Iden>(\\x. (succ x))" $
          typeCheck (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (App (Var 0 (Arr Nat Nat) "x") Zero)) ["x"]) (Lambda Dyn (Succ (Var 0 Dyn "x")) ["x"]))
          `shouldBe` Right (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (App (Var 0 (Arr Nat Nat) "x") Zero)) ["x"]) (Cast (Func (Inject Nat) (Iden Nat)) (Lambda Dyn (Succ (Cast (Project Nat (Label 0)) (Var 0 Dyn "x"))) ["x"])))