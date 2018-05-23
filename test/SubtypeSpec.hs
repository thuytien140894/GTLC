module SubtypeSpec where 

    import Subtype 
    import Type
    import TypeChecker

    import Test.Hspec

    spec :: Spec
    spec = do 
        describe "sorts" $ 
            context "[(b,Nat),(a,Nat)] alphetically" $ 
                it "should be [(a,Nat),(b,Nat)]" $ 
                    sortFields [("b",Nat),("a",Nat)]
                    `shouldBe` [("a",Nat),("b",Nat)]

        describe "subtypes" $ do
            context "{x:{c:Nat,b:Nat,y:Nat}} and {x:{a:Nat}}" $ 
                it "should be false" $ 
                    isSubtype (TRec [("x", TRec [("c",Nat),("b",Nat)]),("y",Nat)]) (TRec [("x", TRec [("a",Nat)])]) 
                    `shouldBe` False

            context "{x:Nat,a:Boolean,b:Boolean} and {x:Nat,y:Nat}" $ 
                it "should be false" $ 
                    isSubtype (TRec [("x", Nat), ("a", Boolean), ("b", Boolean)]) (TRec [("x", Nat), ("y", Nat)]) 
                    `shouldBe` False

            context "{a:Boolean,y:Nat,x:Nat} and {x:Nat,y:Nat}" $ 
                it "should be true" $ 
                    isSubtype (TRec [("a", Boolean), ("y", Nat), ("x", Nat)]) (TRec [("x", Nat), ("y", Nat)]) 
                    `shouldBe` True

            context "{z:Boolean} and {z:Boolean}" $ 
                it "should be true" $ 
                    isSubtype (TRec [("z", Boolean)]) (TRec [("z", Boolean)])
                    `shouldBe` True 
                        
            context "{y:Nat}->{z:Boolean} and {x:Nat,y:Nat}->{z:Boolean}" $ 
                it "should be true" $ 
                    isSubtype (Arr (TRec [("y",Nat)]) (TRec [("z", Boolean)])) (Arr (TRec [("x",Nat),("y",Nat)]) (TRec [("z", Boolean)]))
                    `shouldBe` True