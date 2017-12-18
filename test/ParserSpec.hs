module ParserSpec where

    import Test.Hspec
    import Types
    import Syntax
    import Parser

    spec :: Spec
    spec = 
      describe "parses" $ do
        context "true" $ 
            it "should be \"Tru\"" $ 
              parseExpr "true" `shouldBe` 
              Right (Tru)

        context "(\\ x : Nat->Nat . x z) (\\ y : Nat . y)" $ 
          it "should be \"App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) x) (Var 1 TUnit z)) [x]) (Lambda Nat (Var 0 Nat y) [y])\"" $ 
            parseExpr "(\\ x : Nat->Nat . x z) (\\ y : Nat . y)" `shouldBe` 
            Right (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (Var 1 TUnit "z")) ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"]))

        context "\\ x : Nat.\\ y : Nat . x y" $ 
          it "should be \"Lambda Nat (Lambda Nat (App (Var 1 Nat x) (Var 0 Nat y)) [y]) [x,y]\"" $ 
            parseExpr "(\\ x : Nat.\\ y : Nat . x y)" `shouldBe` 
            Right (Lambda Nat (Lambda Nat (App (Var 1 Nat "x") (Var 0 Nat "y")) ["y"]) ["x","y"])

        context "\\ x : Nat->Nat->Nat.\\ y : Nat . x y" $ 
          it "should be \"Lambda (Arr Nat (Arr Nat Nat)) (Lambda Nat (App (Var 1 Nat x) (Var 0 Nat y)) [y]) [x,y]\"" $ 
            parseExpr "(\\ x : Nat->Nat->Nat.\\ y : Nat . x y)" `shouldBe` 
            Right (Lambda (Arr Nat (Arr Nat Nat)) (Lambda Nat (App (Var 1 Nat "x") (Var 0 Nat "y")) ["y"]) ["x","y"])

        context "\\x: Nat. \\ y : Nat . iszero 0" $ 
          it "should be \"Lambda Nat (Lambda Nat (IsZero Zero) [y]) [x,y]\"" $ 
            parseExpr "(\\x: Nat. \\ y : Nat . iszero 0)" `shouldBe` 
            Right (Lambda Nat (Lambda Nat (IsZero Zero) ["y"]) ["x","y"])

        context "(\\x : Nat->Nat . x) (\\y : Nat . y)" $ 
          it "should be \"App (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) x) [x]) (Lambda Nat (Var 0 Nat y) [y])\"" $ 
            parseExpr "(\\x : Nat->Nat . x) (\\y : Nat . y)" `shouldBe` 
            Right (App (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) "x") ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"]))
          
        context "{x=true,y=false}" $ 
          it "should be \"Rec [(x,Tru),(y,Fls)]\"" $ 
            parseExpr "{x  =true, y=false}" `shouldBe` Right (Rec [("x",Tru),("y",Fls)])

        context "(\\x:Top. x) (\\x:Top. x)" $ 
          it "should be \"App (Lambda Top (Var 0 Top x) [x]) (Lambda Top (Var 0 Top x) [x])\"" $ 
            parseExpr "(\\x:Top. x) (\\x:Top. x)" `shouldBe` Right (App (Lambda Top (Var 0 Top "x") ["x"]) (Lambda Top (Var 0 Top "x") ["x"]))
  
        context "{x=true,y=false}.x" $ 
          it "should be \"Proj (Rec [(x,Tru),(y,Fls)]) x\"" $ 
            parseExpr "{x  =true, y=false}.x" `shouldBe` Right (Proj (Rec [("x",Tru),("y",Fls)]) "x")

        context "\\x: {a:Nat,b:{c:Bool}} . x" $ 
          it "should be \"Lambda (TRec [(a,Nat),(b,TRec [(c,Bool)])]) (Var 0 (TRec [(a,Nat),(b,TRec [(c,Bool)])]) x) [x]\"" $ 
            parseExpr "\\x: {a:Nat,b:{c:Bool}}. x" `shouldBe` 
            Right (Lambda (TRec [("a",Nat),("b",TRec [("c",Bool)])]) (Var 0 (TRec [("a",Nat),("b",TRec [("c",Bool)])]) "x") ["x"])
  
        context "(\\x: {a:Nat,b:{c:Bool}}. x) {a=succ 0, b={c=true,d=0}, e=false}" $ 
          it "should be \"App (Lambda (TRec [(a,Nat),(b,TRec [(c,Bool)])]) (Var 0 (TRec [(a,Nat),(b,TRec[(c,Bool)])]) x) [x]) (Rec [(a,Succ Zero),(b,Rec [(c,Tru),(d,Zero)]),(e,Fls)])\"" $ 
            parseExpr "(\\x: {a:Nat,b:{c:Bool}} . x) {a=succ 0, b={c=true,d=0}, e=false}" `shouldBe` 
            Right (App (Lambda (TRec [("a",Nat),("b",TRec [("c",Bool)])]) (Var 0 (TRec [("a",Nat),("b",TRec[("c",Bool)])]) "x") ["x"]) (Rec [("a",Succ Zero),("b",Rec [("c",Tru),("d",Zero)]),("e",Fls)]))