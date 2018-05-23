module CoercionSpec where

    import Coercion
    import Syntax
    import Type

    import Test.Hspec

    spec :: Spec
    spec = do
        describe "reduces coercion" $ do 
            context "Identity; Boolean!" $
                it "should be Boolean!" $
                    reduceCoercion (Seq (Iden Boolean) (Inject Boolean)) 
                    `shouldBe` Inject Boolean

            context "Nat!; Boolean?" $
                it "should fail" $
                    reduceCoercion (Seq (Inject Nat) (Project Boolean (Label 0))) 
                    `shouldBe` Fail Nat Boolean (Label 0)

            context "Nat!; Nat?" $
                it "should be Identity" $
                    normalize (Seq (Inject Nat) (Project Nat (Label 0))) 
                    `shouldBe` Iden Nat

            context "Nat?->Boolean!; Nat!->Boolean?" $
                it "should be Iden->Iden" $
                    normalize (Seq (Func (Project Nat (Label 0)) (Inject Boolean)) (Func (Inject Nat) (Project Boolean (Label 1)))) 
                    `shouldBe` Iden Nat
            
        describe "get coercion types" $ do
            context "Iden Boolean" $ 
                it "should be (Boolean, Boolean)" $ 
                    getCoercionTypes (Iden Boolean) 
                    `shouldBe` (Boolean, Boolean)
            
            context "Fail Nat Boolean" $
                it "should be (Nat, Boolean)" $
                    getCoercionTypes (Fail Nat Boolean (Label 0)) 
                    `shouldBe` (Nat, Boolean)

            context "Inject Nat" $ 
                it "should be (Nat, Dyn)" $ 
                    getCoercionTypes (Inject Nat) 
                    `shouldBe` (Nat, Dyn)

            context "Project Boolean" $ 
                it "should be (Dyn, Boolean)" $ 
                    getCoercionTypes (Project Boolean (Label 0)) 
                    `shouldBe` (Dyn, Boolean)

            context "Func (Inject Boolean) (Project Nat)" $ 
                it "should be (Arr Dyn Dyn, Arr Boolean Nat)" $ 
                    getCoercionTypes (Func (Inject Boolean) (Project Nat (Label 0))) 
                    `shouldBe` (Arr Dyn Dyn, Arr Boolean Nat)

            context "CRef (Inject Nat) (Project Nat)" $ 
                it "should be (Ref Dyn, Ref Nat)" $ 
                    getCoercionTypes (CRef (Inject Nat) (Project Nat (Label 0))) 
                    `shouldBe` (TRef Dyn,TRef Nat)

            context "Seq (Iden Boolean) (Project Nat)" $ 
                it "should be (Boolean, Nat)" $ 
                    getCoercionTypes (Seq (Iden Boolean) (Project Nat (Label 0))) 
                    `shouldBe` (Boolean, Nat)

        describe "is normalized" $ do
            context "Iden;<Nat?-Nat!>" $ 
                it "should be true" $ 
                    isNormalized (Seq (Project Nat (Label 2)) (Func (Inject Nat) (Project Nat (Label 1)))) 
                    `shouldBe` True
            
            context "Boolean?;(Nat?->Nat!);Nat!" $
                it "should be true" $
                    isNormalized (Seq (Seq (Project Nat (Label 2)) (Func (Inject Nat) (Project Nat (Label 1)))) (Inject Nat))
                    `shouldBe` True

            context "Boolean?;(Nat?->Nat!);Nat!" $
                it "should be true" $
                    isNormalized (Seq (Project Nat (Label 2)) (Seq (Func (Inject Nat) (Project Nat (Label 1))) (Inject Nat)))
                    `shouldBe` True
