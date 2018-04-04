module CoercionSpec where

  import Test.Hspec
  import Syntax
  import Types
  import Coercion

  spec :: Spec
  spec = do
    describe "coerces" $ do
      context "Bool to Dynamic" $
        it "should be Bool!" $
          coerce Bool Dyn 0 `shouldBe` (Inject Bool, 0)

      context "Dynamic to Nat" $
        it "should be Nat?" $
          coerce Dyn Nat 0 `shouldBe` (Project Nat 0, 1)

      context "Bool to Bool" $
        it "should be Identity" $
          coerce Bool Bool 0 `shouldBe` (Iden Bool, 0)

      context "Dynamic to Bool->Nat" $
        it "should be Bool!->Nat?" $
          coerce Dyn (Arr Bool Nat) 0 `shouldBe` (Func (Inject Bool) (Project Nat 0), 1)

      context "Bool->Nat to Dynamic" $
        it "should be Bool?->Nat!" $
          coerce (Arr Bool Nat) Dyn 0 `shouldBe` (Func (Project Bool 0) (Inject Nat), 1)

    describe "reduces coercion" $ do 
      context "Identity; Bool!" $
        it "should be Bool!" $
          reduceCoercion (Seq (Iden Bool) (Inject Bool)) `shouldBe` Inject Bool

      context "Nat!; Bool?" $
        it "should fail" $
          reduceCoercion (Seq (Inject Nat) (Project Bool 0)) `shouldBe` Fail Nat Bool 0

      context "Nat!; Nat?" $
        it "should be Identity" $
          normalize (Seq (Inject Nat) (Project Nat 0)) `shouldBe` Iden Nat

      context "Nat?->Bool!; Nat!->Bool?" $
        it "should be Iden->Iden" $
          normalize (Seq (Func (Project Nat 0) (Inject Bool)) (Func (Inject Nat) (Project Bool 0))) 
          `shouldBe` Func (Iden Nat) (Iden Bool)
    
    describe "get coercion types" $ do
      context "Iden Bool" $ 
        it "should be (Bool, Bool)" $ 
          getCoercionTypes (Iden Bool) `shouldBe` (Bool, Bool)
      
      context "Fail Nat Bool" $
        it "should be (Nat, Bool)" $
          getCoercionTypes (Fail Nat Bool 0) `shouldBe` (Nat, Bool)

      context "Inject Nat" $ 
        it "should be (Nat, Dyn)" $ 
          getCoercionTypes (Inject Nat) `shouldBe` (Nat, Dyn)

      context "Project Bool" $ 
        it "should be (Dyn, Bool)" $ 
          getCoercionTypes (Project Bool 0) `shouldBe` (Dyn, Bool)

      context "Func (Inject Bool) (Project Nat)" $ 
        it "should be (Arr Dyn Dyn, Arr Bool Nat)" $ 
          getCoercionTypes (Func (Inject Bool) (Project Nat 0)) `shouldBe` (Arr Dyn Dyn, Arr Bool Nat)

      context "Seq (Iden Bool) (Project Nat)" $ 
        it "should be (Bool, Nat)" $ 
          getCoercionTypes (Seq (Iden Bool) (Project Nat 0)) `shouldBe` (Bool, Nat)

