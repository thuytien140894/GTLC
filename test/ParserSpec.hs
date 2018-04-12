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
          Right Tru

      context "if ref true then x:=0 else !x" $ 
        it "should be \"If (Ref Tru) (Assign (Var (-1) TUnit c) Zero) (Deref (Var (-1) TUnit x))\"" $ 
          parseExpr "if ref true then x:=0 else !x" `shouldBe` 
          Right (If (Ref Tru) (Assign (Var (-1) TUnit "x") Zero) (Deref (Var (-1) TUnit "x")))

      context "!((\\x. \\y. x:=true) ref false)" $ 
        it "should be \"Deref (App (Lambda Dyn (Lambda Dyn (Assign (Var 1 Dyn x) Tru) [y]) [x,y]) (Ref Fls))\"" $ 
          parseExpr "!((\\x. \\y. x:=true) ref false)" `shouldBe` 
          Right (Deref (App (Lambda Dyn (Lambda Dyn (Assign (Var 1 Dyn "x") Tru) ["y"]) ["x","y"]) (Ref Fls)))
      
      context "(\\m. (\\x. x 0) m)" $ 
        it "should be \"\\. (\\.0 zero) 0\"" $ 
          parseExpr "(\\m. (\\x. x 0) m)" `shouldBe` 
          Right (Lambda Dyn (App (Lambda Dyn (App (Var 0 Dyn "x") Zero) ["x"]) (Var 0 Dyn "m")) ["m","x"])

      context "(\\m. ((\\x. (x 0)) m)) (\\y:Nat. succ y)" $ 
        it "should be \"(\\m. (\\x. x 0) m) (\\:Nat. succ 0)\"" $ 
          parseExpr "(\\m. ((\\x. (x 0)) m)) (\\y:Nat. succ y)" `shouldBe` 
          Right (App (Lambda Dyn (App (Lambda Dyn (App (Var 0 Dyn "x") Zero) ["x"]) (Var 0 Dyn "m")) ["m","x"]) (Lambda Nat (Succ (Var 0 Nat "y")) ["y"]))

      context "\\f. (\\x. f (\\y. (x x) y)) (\\x. f (\\y. (x x) y))"$ 
        it "should be \"\\. (\\. 1 (\\. (1 1) 0)) (\\. 1 (\\. (1 1) 0))\"" $ 
          parseExpr "\\f. (\\x. f (\\y. (x x) y)) (\\x. f (\\y. (x x) y))" `shouldBe` 
          Right (Lambda Dyn (App (Lambda Dyn (App (Var 1 Dyn "f") (Lambda Dyn (App (App (Var 1 Dyn "x") (Var 1 Dyn "x")) (Var 0 Dyn "y"))["y"])) ["x","y"]) (Lambda Dyn (App (Var 1 Dyn "f") (Lambda Dyn (App (App (Var 1 Dyn "x") (Var 1 Dyn "x")) (Var 0 Dyn "y")) ["y"])) ["x","y"])) ["f","x","y","x","y"])

      context "(\\x. succ x) true" $ 
        it "should be \"Lambda Dyn (Succ (Var 0 Dyn x)) [x]) Tru\"" $ 
          parseExpr "(\\x. succ x) true" `shouldBe` 
          Right (App (Lambda Dyn (Succ (Var 0 Dyn "x")) ["x"]) Tru)

      context "(\\x:Bool->Nat. succ 0) (\\x:Nat. x)" $ 
        it "should be \"Lambda Dyn (Var 0 Dyn x) [x]\""$ 
          parseExpr "(\\x:Bool->Nat. succ 0) (\\x:Nat. x)" `shouldBe` 
          Right (App (Lambda (Arr Bool Nat) (Succ Zero) ["x"]) (Lambda Nat (Var 0 Nat "x") ["x"]))

      context "(\\x:Dyn->Nat. (x 0)) (\\x:Nat. x)" $ 
        it "should be \"App (Lambda (Arr Dyn Nat) (App (Var 0 (Arr Dyn Nat) x) Zero) [x]) (Lambda Nat (Var 0 Nat x) [x])\""$ 
          parseExpr "(\\x:Dyn->Nat. (x 0)) (\\x:Nat. x)" `shouldBe` 
          Right (App (Lambda (Arr Dyn Nat) (App (Var 0 (Arr Dyn Nat) "x") Zero) ["x"]) (Lambda Nat (Var 0 Nat "x") ["x"]))

      context "(\\x:Nat->Bool. (x (succ 0))) (\\x:Nat. (iszero x))" $ 
        it "should be \"App (Lambda (Arr Nat Bool) (App (Var 0 (Arr Nat Bool) x) (Succ Zero)) [x]) (Lambda Nat (IsZero (Var 0 Nat x)) [x])\""$ 
          parseExpr "(\\x:Nat->Bool. (x (succ 0))) (\\x:Nat. (iszero x))" `shouldBe` 
          Right (App (Lambda (Arr Nat Bool) (App (Var 0 (Arr Nat Bool) "x") (Succ Zero)) ["x"]) (Lambda Nat (IsZero (Var 0 Nat "x")) ["x"]))

      context "(\\x. (x 0)) (\\x:Nat. (succ x))" $ 
        it "should be \"App (Lambda Dyn (App (Var 0 Dyn x) Zero) [x]) (Lambda Nat (Succ (Var 0 Nat x)) [x])\""$ 
          parseExpr "(\\x. (x 0)) (\\x:Nat. (succ x))" `shouldBe` 
          Right (App (Lambda Dyn (App (Var 0 Dyn "x") Zero) ["x"]) (Lambda Nat (Succ (Var 0 Nat "x")) ["x"]))

      context "(\\x:Nat->Nat. x (x 0)) (\\x. (succ x))" $ 
        it "should be \"App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) x) (App (Var 0 (Arr Nat Nat) x) Zero)) [x]) (Lambda Dyn (Succ (Var 0 Dyn x)) [x])\""$ 
          parseExpr "(\\x:Nat->Nat. x (x 0)) (\\x. (succ x))" `shouldBe` 
          Right (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (App (Var 0 (Arr Nat Nat) "x") Zero)) ["x"]) (Lambda Dyn (Succ (Var 0 Dyn "x")) ["x"]))
          
      context "(\\ x : Dyn->Nat . x z) (\\ y. y)" $ 
        it "should be \"App (Lambda (Arr Dyn Nat) (App (Var 0 (Arr Dyn Nat) x) (Var 1 TUnit z)) [x]) (Lambda Dyn (Var 0 Dyn y) [y])\"" $ 
          parseExpr "(\\ x : Dyn->Nat . x z) (\\ y. y)" `shouldBe` 
          Right (App (Lambda (Arr Dyn Nat) (App (Var 0 (Arr Dyn Nat) "x") (Var 1 TUnit "z")) ["x"]) (Lambda Dyn (Var 0 Dyn "y") ["y"]))

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
          parseExpr "(\\ x :Nat->Nat->Nat.\\ y : Nat .x y)" `shouldBe` 
          Right (Lambda (Arr Nat (Arr Nat Nat)) (Lambda Nat (App (Var 1 Nat "x") (Var 0 Nat "y")) ["y"]) ["x","y"])

      context "\\x: Nat. \\ y : Nat . iszero 0" $ 
        it "should be \"Lambda Nat (Lambda Nat (IsZero Zero) [y]) [x,y]\"" $ 
          parseExpr "(\\x: Nat. \\ y : Nat . iszero 0)" `shouldBe` 
          Right (Lambda Nat (Lambda Nat (IsZero Zero) ["y"]) ["x","y"])

      context "(\\x : Nat->Nat . x) (\\y : Nat . y)" $ 
        it "should be \"App (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) x) [x]) (Lambda Nat (Var 0 Nat y) [y])\"" $ 
          parseExpr "(\\x : Nat->Nat . x) (\\y : Nat . y)" `shouldBe` 
          Right (App (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) "x") ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"]))

      context "(\\m. ((\\x:Nat->Nat. (x 0)) m)) (\\y:Nat. succ y)" $ 
        it "should be \"App (Lambda (Arr Nat Nat) (App (Lambda (Arr Nat Nat) (App (Var 1 (Arr Nat Nat) x) Zero) [x]) (Var 0 (Arr Nat Nat) m)) [m]) (Lambda Nat (Succ (Var 0 Nat y)) [y]\"" $ 
          parseExpr "(\\m. ((\\x:Nat->Nat. (x 0)) m)) (\\y:Nat. succ y)" `shouldBe` 
          Right (App (Lambda Dyn (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Var 0 Dyn "m")) ["m","x"]) (Lambda Nat (Succ (Var 0 Nat "y")) ["y"]))

      context "if (\\x. iszero 0) (succ 0) then 0 else (\\x:Nat->Nat. x 0) (\\y. succ y)" $ 
        it "should be \"If (App (Lambda Dyn (IsZero Zero) [x]) (Succ Zero)) Zero (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) x) Zero) [x]) (Lambda Dyn (Succ (Var 0 Dyn y)) [y]))\"" $ 
          parseExpr "if (\\x. iszero 0) (succ 0) then 0 else (\\x:Nat->Nat. x 0) (\\y. succ y)" `shouldBe` 
          Right (If (App (Lambda Dyn (IsZero Zero) ["x"]) (Succ Zero)) Zero (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Lambda Dyn (Succ (Var 0 Dyn "y")) ["y"])))

      context "((\\m. if (\\x. iszero x) m then (\\x. succ x) else (\\x. pred x)) 0) true" $ 
        it "should be \"App (App (Lambda Dyn (If (App (Lambda Dyn (IsZero (Var 0 Dyn x)) [x]) (Var 0 Dyn m)) (Lambda Dyn (Succ (Var 0Dyn x)) [x]) (Lambda Dyn (Pred (Var 0 Dyn x)) [x])) [m]) Zero) Tru\"" $ 
          parseExpr "((\\m. if (\\x. iszero x) m then (\\x. succ x) else (\\x. pred x)) 0) true" `shouldBe` 
          Right (App (App (Lambda Dyn (If (App (Lambda Dyn (IsZero (Var 0 Dyn "x")) ["x"]) (Var 0 Dyn "m")) (Lambda Dyn (Succ (Var 0Dyn "x")) ["x"]) (Lambda Dyn (Pred (Var 0 Dyn "x")) ["x"])) ["m"]) Zero) Tru)

      context "(\\n. (\\m. if (\\x. iszero x) n then m 0 else m (succ 0)) (\\y. succ y))) 0" $ 
        it "should be \"App (Lambda Dyn (App (Lambda Dyn (If (App (Lambda Dyn (IsZero (Var 0 Dyn x)) [x]) (Var 1 Dyn n)) (App (Var 0 Dyn m) Zero) (App (Var 0 Dyn m) (Succ Zero))) [m]) (Lambda Dyn (Succ (Var 0 Dyn y)) [y])) [n,m,y]) Zero\"" $ 
          parseExpr "(\\n. (\\m. if (\\x. iszero x) n then m 0 else m (succ 0)) (\\y. succ y)) 0" `shouldBe` 
          Right (App (Lambda Dyn (App (Lambda Dyn (If (App (Lambda Dyn (IsZero (Var 0 Dyn "x")) ["x"]) (Var 1 Dyn "n")) (App (Var 0 Dyn "m") Zero) (App (Var 0 Dyn "m") (Succ Zero))) ["m"]) (Lambda Dyn (Succ (Var 0 Dyn "y")) ["y"])) ["n","m","y"]) Zero)
      
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