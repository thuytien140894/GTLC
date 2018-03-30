module TypeCheckerSpec where 

  import Test.Hspec
  import TypeChecker
  import Syntax
  import Types
  import Errors

  spec :: Spec
  spec = 
    describe "typechecks" $ do
      context "\\x:Nat. \\y:Nat. iszero 0" $ 
        it "should be Nat->Nat->Bool" $ 
          typeOf (Lambda Nat (Lambda Nat (IsZero Zero) ["y"]) ["x","y"]) `shouldBe` 
          Right (Arr Nat (Arr Nat Bool))

      context "(\\x:Nat->Nat. (x 0)) (\\x:Nat. (succ x))" $ 
        it "should be Nat" $ 
          typeOf (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Lambda Nat (Succ (Var 0 Nat "x")) ["x"])) `shouldBe` 
          Right Nat

      context "\\x:Nat->Nat. x" $ 
        it "should be (Nat->Nat)->(Nat->Nat)" $ 
          typeOf (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) "x") ["x"]) `shouldBe` 
          Right (Arr (Arr Nat Nat) (Arr Nat Nat))

      context "(\\x:Nat->Nat. x) (\\y:Nat. y)" $ 
        it "should be Nat->Nat" $ 
          typeOf (App (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) "x") ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"])) `shouldBe` 
          Right (Arr Nat Nat)

      context "(\\x: {a:Nat,b:{c:Bool}}. x) {a:succ 0,b:{c:true,d:0},e:false}" $ 
        it "should be {a:Nat,b:{c:Bool}}" $ 
          typeOf (App (Lambda (TRec [("a",Nat),("b",TRec [("c",Bool)])]) (Var 0 (TRec [("a",Nat),("b",TRec[("c",Bool)])]) "x") ["x"]) (Rec [("a",Succ Zero),("b",Rec [("c",Tru),("d",Zero)]),("e",Fls)])) `shouldBe` 
          Right (TRec [("a",Nat),("b",TRec [("c",Bool)])])

      context "{x:succ 0,y:true}" $ 
        it "should be {x:Nat,y:Bool}" $ 
          typeOf (Rec [("x", Succ Zero), ("y", Tru)]) `shouldBe` Right (TRec [("x",Nat),("y",Bool)])

      context "{x:if true then (iszero 0) else false,y:succ 0}.x" $ 
        it "should be Bool" $ 
          typeOf (Proj (Rec [("x", If Tru (IsZero Zero) Fls), ("y", Succ Zero)]) "x") `shouldBe` Right Bool

      context "(\\x:Nat->Nat. x) (\\y:Bool. y)" $ 
        it "should be Mismatch (Bool->Bool) (Nat->Nat)" $ 
            typeOf (App (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) "x") ["x"]) (Lambda Bool (Var 0 Bool "y") ["y"])) `shouldBe` 
            Left (Mismatch (Arr Bool Bool) (Arr Nat Nat))

      context "if true else true then 0" $ 
        it "should be Difference Bool Nat" $ 
          typeOf (If Tru Tru Zero) `shouldBe` 
          Left (Difference Bool Nat)

      context "(\\x:Nat->Nat. x (succ 0)) (\\y:Nat. y)" $ 
        it "should be Nat" $ 
          typeOf (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (Succ Zero)) ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"])) `shouldBe` 
          Right Nat

      context "\\x:Top. x" $ 
        it "should be Top->Top" $ 
          typeOf (Lambda Top (Var 0 Top "x")  ["x"]) `shouldBe` 
          Right (Arr Top Top)

      context "(\\x:Top. x) (\\x:Top. x)" $ 
        it "should be Top" $ 
          typeOf (App (Lambda Top (Var 0 Top "x") ["x"]) (Lambda Top (Var 0 Top "x") ["x"])) `shouldBe` 
          Right Top

      context "(\\x:Nat. x) (\\x:Nat. x)" $ 
        it "should be Mismatch (Nat->Nat) Nat" $ 
          typeOf (App (Lambda Nat (Var 0 Nat "x") ["x"]) (Lambda Nat (Var 0 Nat "x") ["x"])) `shouldBe` 
          Left (Mismatch (Arr Nat Nat) Nat)

      context "succ false" $ 
        it "should be NotNat" $ 
            typeOf (Succ Fls) `shouldBe`
            Left (NotNat Bool)

      context "<Bool!> True" $ 
        it "should be Dynamic" $ 
          typeOf (Cast (Inject Bool) Tru) `shouldBe` Right Dyn

      context "<Bool?->Nat!> \\x:Bool. 0" $ 
        it "should be Bool->Nat" $ 
            typeOf (Cast (Func (Project Bool 0) (Inject Nat)) (Lambda Bool Zero ["x"])) `shouldBe`
            Right (Arr Dyn Dyn)
      
      context "<Nat!> False" $ 
        it "should be IllegalCast Bool Nat" $ 
            typeOf (Cast (Inject Nat) Fls) `shouldBe`
            Left (IllegalCast Bool Nat)

      context "\\x:Nat->Nat. x (x 0)" $ 
        it "should be (Nat->Nat)->Nat" $ 
            typeOf (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (App (Var 0 (Arr Nat Nat) "x") Zero)) ["x"]) `shouldBe`
            Right (Arr (Arr Nat Nat) Nat)