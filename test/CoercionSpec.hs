module CoercionSpec where

    import Coercion
    import Syntax
    import Type

    import Test.Hspec

    spec :: Spec
    spec = do
        describe "reduces coercion" $ do 
            context "Identity; Bool!" $
                it "should be Bool!" $
                reduceCoercion (Seq (Iden Bool) (Inject Bool)) 
                `shouldBe` Inject Bool

            context "Nat!; Bool?" $
                it "should fail" $
                reduceCoercion (Seq (Inject Nat) (Project Bool (Label 0))) 
                `shouldBe` Fail Nat Bool (Label 0)

            context "Nat!; Nat?" $
                it "should be Identity" $
                normalize (Seq (Inject Nat) (Project Nat (Label 0))) 
                `shouldBe` Iden Nat

            context "Nat?->Bool!; Nat!->Bool?" $
                it "should be Iden->Iden" $
                normalize (Seq (Func (Project Nat (Label 0)) (Inject Bool)) (Func (Inject Nat) (Project Bool (Label 1)))) 
                `shouldBe` Iden Nat
            
        describe "get coercion types" $ do
            context "Iden Bool" $ 
                it "should be (Bool, Bool)" $ 
                getCoercionTypes (Iden Bool) 
                `shouldBe` (Bool, Bool)
            
            context "Fail Nat Bool" $
                it "should be (Nat, Bool)" $
                getCoercionTypes (Fail Nat Bool (Label 0)) 
                `shouldBe` (Nat, Bool)

            context "Inject Nat" $ 
                it "should be (Nat, Dyn)" $ 
                getCoercionTypes (Inject Nat) 
                `shouldBe` (Nat, Dyn)

            context "Project Bool" $ 
                it "should be (Dyn, Bool)" $ 
                getCoercionTypes (Project Bool (Label 0)) 
                `shouldBe` (Dyn, Bool)

            context "Func (Inject Bool) (Project Nat)" $ 
                it "should be (Arr Dyn Dyn, Arr Bool Nat)" $ 
                getCoercionTypes (Func (Inject Bool) (Project Nat (Label 0))) 
                `shouldBe` (Arr Dyn Dyn, Arr Bool Nat)

            context "CRef (Inject Nat) (Project Nat)" $ 
                it "should be (Ref Dyn, Ref Nat)" $ 
                getCoercionTypes (CRef (Inject Nat) (Project Nat (Label 0))) 
                `shouldBe` (TRef Dyn,TRef Nat)

            context "Seq (Iden Bool) (Project Nat)" $ 
                it "should be (Bool, Nat)" $ 
                getCoercionTypes (Seq (Iden Bool) (Project Nat (Label 0))) 
                `shouldBe` (Bool, Nat)

        describe "is normalized" $ do
            context "Iden;<Nat?-Nat!>" $ 
                it "should be true" $ 
                isNormalized (Seq (Project Nat (Label 2)) (Func (Inject Nat) (Project Nat (Label 1)))) 
                `shouldBe` True
            
            context "Bool?;(Nat?->Nat!);Nat!" $
                it "should be true" $
                isNormalized (Seq (Seq (Project Nat (Label 2)) (Func (Inject Nat) (Project Nat (Label 1)))) (Inject Nat))
                `shouldBe` True

            context "Bool?;(Nat?->Nat!);Nat!" $
                it "should be true" $
                isNormalized (Seq (Project Nat (Label 2)) (Seq (Func (Inject Nat) (Project Nat (Label 1))) (Inject Nat)))
                `shouldBe` True
