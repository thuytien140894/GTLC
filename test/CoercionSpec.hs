module CoercionSpec where

  import Test.Hspec
  import Syntax
  import Types
  import Coercion

  spec :: Spec
  spec = 
    describe "get coercion types" $ do
      context "Iden Bool" $
        it "should be (Bool, Bool)" $
          getTypes (Iden Bool) `shouldBe` (Bool, Bool)
      
      context "Fail Nat Bool" $
        it "should be (Nat, Bool)" $
          getTypes (Fail Nat Bool) `shouldBe` (Nat, Bool)

      context "Inject Nat" $ 
        it "should be (Nat, Dyn)" $ 
          getTypes (Inject Nat) `shouldBe` (Nat, Dyn)

      context "Project Bool" $ 
        it "should be (Dyn, Bool)" $ 
          getTypes (Project Bool) `shouldBe` (Dyn, Bool)

      context "Func (Inject Bool) (Project Nat)" $ 
        it "should be (Arr Dyn Dyn, Arr Bool Nat)" $ 
          getTypes (Func (Inject Bool) (Project Nat)) `shouldBe` (Arr Dyn Dyn, Arr Bool Nat)

      context "Seq (Iden Bool) (Project Nat)" $ 
        it "should be (Bool, Nat)" $ 
          getTypes (Seq (Iden Bool) (Project Nat)) `shouldBe` (Bool, Nat)

