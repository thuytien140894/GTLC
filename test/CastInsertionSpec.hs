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
          
      context "(\\x:Dyn->Nat. (x 0)) (\\x:Nat. x)" $
        it "should be \"App (Lambda (Arr Dyn Nat) (App (Var 0 (Arr Dyn Nat) x) Zero) [x]) (Cast (Func (Project Nat) (Iden Nat)) (Lambda Nat (Var 0 Nat x) [x]))\"" $ 
          insertCast (App (Lambda (Arr Dyn Nat) (App (Var 0 (Arr Dyn Nat) "x") Zero) ["x"]) (Lambda Nat (Var 0 Nat "x") ["x"])) `shouldBe` 
          Right (App (Lambda (Arr Dyn Nat) (App (Var 0 (Arr Dyn Nat) "x") Zero) ["x"]) (Cast (Func (Project Nat) (Iden Nat)) (Lambda Nat (Var 0 Nat "x") ["x"])))