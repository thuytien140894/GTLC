module SubtyperSpec where 

  import Test.Hspec
  import Subtype 
  import TypeChecker
  import Types

  spec :: Spec
  spec = 
    describe "subtypes" $ do
      context "{x:{c:Nat,b:Nat,y:Nat}} and {x:{a:Nat}}" $ 
        it "should be false" $ 
          isSubtype (TRec [("x", TRec [("c",Nat),("b",Nat)]),("y",Nat)]) (TRec [("x", TRec [("a",Nat)])]) 
          `shouldBe` False

      context "by sorting [(b,Nat),(a,Nat)] alphetically" $ 
        it "should be [(a,Nat),(b,Nat)]" $ 
          sortFields [("b",Nat),("a",Nat)]
          `shouldBe` [("a",Nat),("b",Nat)]

      context "{x:Nat,a:Bool,b:Bool} and {x:Nat,y:Nat}" $ 
        it "should be false" $ 
          isSubtype (TRec [("x", Nat), ("a", Bool), ("b", Bool)]) (TRec [("x", Nat), ("y", Nat)]) 
          `shouldBe` False

      context "{a:Bool,y:Nat,x:Nat} and {x:Nat,y:Nat}" $ 
        it "should be true" $ 
          isSubtype (TRec [("a", Bool), ("y", Nat), ("x", Nat)]) (TRec [("x", Nat), ("y", Nat)]) 
          `shouldBe` True

      context "{z:Bool} and {z:Bool}" $ 
        it "should be true" $ 
          isSubtype (TRec [("z", Bool)]) (TRec [("z", Bool)])
          `shouldBe` True 
                
      context "{y:Nat}->{z:Bool} and {x:Nat,y:Nat}->{z:Bool}" $ 
        it "should be true" $ 
          isSubtype (Arr (TRec [("y",Nat)]) (TRec [("z", Bool)])) (Arr (TRec [("x",Nat),("y",Nat)]) (TRec [("z", Bool)]))
          `shouldBe` True