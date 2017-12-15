module PrinterSpec where 

    import Test.Hspec
    import Types
    import Syntax
    import Prettier
    import TypeErrors

    spec :: Spec
    spec = 
      describe "prints" $ do
        context "Mismatch Bool Nat" $ 
          it "should be \"Type mismatch for function argument\n    got: Bool\n    but expected: Nat\"" $ 
            printPretty (Mismatch Bool Nat) `shouldBe` 
            "Type mismatch for function argument\n    got: Bool\n    but expected: Nat"

        context "Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) y) [y]" $ 
          it "should be \"\\ y : Nat->Nat . y\"" $ 
            printPretty (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) "y") ["y"]) `shouldBe` 
            "\\ y : Nat->Nat . y"

        context "NotRecord Tru" $ 
          it "should be \"Couldn't perform projection on non-record expression: true\"" $ 
            printPretty (NotRecord Tru) `shouldBe` "Couldn't perform projection on non-record expression: true"
                    
        context "App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) x) (Var 1 TUnit z)) [x]) (Lambda Nat (Var 0 Nat y) [y])" $ 
          it "should be \"(\\ x : Nat->Nat . x z) (\\ y : Nat . y)\"" $ 
            printPretty (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (Var 1 TUnit "z")) ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"])) 
            `shouldBe` "(\\ x : Nat->Nat . x z) (\\ y : Nat . y)"
              
        context "App (Lambda (TRec [(a,Nat),(b,TRec [(c,Bool)])]) (Var 0 (TRec [(a,Nat),(b,TRec[(c,Bool)])]) x) [x]) (Rec [(a,Succ Zero),(b,Rec [(c,Tru),(d,Zero)]),(e,Fls)])" $ 
          it "should be \"(\\ x : {a:Nat,b:{c:Bool}} . x) {a=succ 0,b={c=true, d=0},e=false}\"" $ 
            printPretty (App (Lambda (TRec [("a",Nat),("b",TRec [("c",Bool)])]) (Var 0 (TRec [("a",Nat),("b",TRec[("c",Bool)])]) "x") ["x"]) (Rec [("a",Succ Zero),("b",Rec [("c",Tru),("d",Zero)]),("e",Fls)])) 
            `shouldBe` "(\\ x : {a:Nat,b:{c:Bool}} . x) {a=succ 0,b={c=true,d=0},e=false}"