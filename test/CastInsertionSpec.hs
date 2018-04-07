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
          insertCast (App (Lambda Dyn Zero ["x"]) Tru) `shouldBe` 
          Right (App (Lambda Dyn Zero ["x"]) (Cast (Inject Bool) Tru))

      context "(\\x. (x 0)) (\\x:Nat. (succ x))" $
        it "should be (\\x. (x <Nat!>0)) <Nat?->Nat!>(\\x:Nat. (succ <Iden>x))" $ 
          insertCast (App (Lambda Dyn (App (Var 0 Dyn "x") Zero) ["x"]) (Lambda Nat (Succ (Var 0 Nat "x")) ["x"])) `shouldBe` 
          Right (App (Lambda Dyn (App (Var 0 Dyn "x") (Cast (Inject Nat) Zero)) ["x"]) (Cast (Seq (Func (Project Nat 0) (Inject Nat)) (Inject (Arr Dyn Dyn))) (Lambda Nat (Succ (Var 0 Nat "x")) ["x"])))
          
      context "(\\x:Dyn->Nat. (x 0)) (\\x:Nat. x)" $
        it "should be (\\x:Dyn->Nat. (x <Nat!>0)) <Nat?->Identity>(\\x:Nat. x)" $ 
          insertCast (App (Lambda (Arr Dyn Nat) (App (Var 0 (Arr Dyn Nat) "x") Zero) ["x"]) (Lambda Nat (Var 0 Nat "x") ["x"])) `shouldBe` 
          Right (App (Lambda (Arr Dyn Nat) (App (Var 0 (Arr Dyn Nat) "x") (Cast (Inject Nat) Zero)) ["x"]) (Cast (Func (Project Nat 0) (Iden Nat)) (Lambda Nat (Var 0 Nat "x") ["x"])))

      context "(\\x. succ x) true" $
        it "should be (\\x. succ <Nat?>x) <Bool!>true" $
          insertCast (App (Lambda Dyn (Succ (Var 0 Dyn "x")) ["x"]) Tru) `shouldBe` 
          Right (App (Lambda Dyn (Succ (Cast (Project Nat 0) (Var 0 Dyn "x"))) ["x"]) (Cast (Inject Bool) Tru))

      context "(\\x:Bool->Nat. succ 0) (\\x:Nat. x)" $
        it "should be (\\x:Bool->Nat. succ <Identity>0) <Fail>(\\x:Nat. x)" $
          insertCast (App (Lambda (Arr Bool Nat) (Succ Zero) ["x"]) (Lambda Nat (Var 0 Nat "x") ["x"])) `shouldBe` 
          Left (Mismatch (Arr Nat Nat) (Arr Bool Nat))

      context "(\\x:Bool. x) 0" $
        it "should be Mismatch" $
          insertCast (App (Lambda Bool (Var 0 Bool "x") ["x"]) Zero)
          `shouldBe` Left (Mismatch Nat Bool)

      context "(\\x:Nat->Nat. x (x 0)) (\\x. (succ x))" $
        it "should be (\\x:Nat->Nat. x <Iden>(x <Iden>0)) <Nat!->Iden>(\\x. (succ x))" $
          insertCast (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (App (Var 0 (Arr Nat Nat) "x") Zero)) ["x"]) (Lambda Dyn (Succ (Var 0 Dyn "x")) ["x"]))
          `shouldBe` Right (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (Cast (Iden Nat) (App (Var 0 (Arr Nat Nat) "x") (Cast (Iden Nat) Zero)))) ["x"]) (Cast (Func (Inject Nat) (Iden Nat)) (Lambda Dyn (Succ (Cast (Project Nat 0) (Var 0 Dyn "x"))) ["x"])))