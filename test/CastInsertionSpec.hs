module CastInsertionSpec where

  import Test.Hspec
  import CastInserter
  import Parser
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
        it "should be (\\x. (x <Nat!>0)) <Nat?->Nat!>(\\x:Nat. (succ x))" $ 
          insertCast (App (Lambda Dyn (App (Var 0 Dyn "x") Zero) ["x"]) (Lambda Nat (Succ (Var 0 Nat "x")) ["x"])) `shouldBe` 
          Right (App (Lambda Dyn (App (Var 0 Dyn "x") (Cast (Inject Nat) Zero)) ["x"]) (Cast (Func (Project Nat) (Inject Nat)) (Lambda Nat (Succ (Cast (Iden Nat) (Var 0 Nat "x"))) ["x"])))
          
      context "(\\x:Dyn->Nat. (x 0)) (\\x:Nat. x)" $
        it "should be (\\x:Dyn->Nat. (x <Nat!>0)) <Nat?->Identity>(\\x:Nat. x)" $ 
          insertCast (App (Lambda (Arr Dyn Nat) (App (Var 0 (Arr Dyn Nat) "x") Zero) ["x"]) (Lambda Nat (Var 0 Nat "x") ["x"])) `shouldBe` 
          Right (App (Lambda (Arr Dyn Nat) (App (Var 0 (Arr Dyn Nat) "x") (Cast (Inject Nat) Zero)) ["x"]) (Cast (Func (Project Nat) (Iden Nat)) (Lambda Nat (Var 0 Nat "x") ["x"])))

      context "(\\x. succ x) true" $
        it "should be (\\x. succ <Nat?>x) <Bool!>true" $
          insertCast (App (Lambda Dyn (Succ (Var 0 Dyn "x")) ["x"]) Tru) `shouldBe` 
          Right (App (Lambda Dyn (Succ (Cast (Project Nat) (Var 0 Dyn "x"))) ["x"]) (Cast (Inject Bool) Tru))