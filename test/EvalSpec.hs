module EvalSpec where

import Test.Hspec
import Syntax
import Parser
import Evaluator
import Prettier
import Types
import TypeErrors
import TypeChecker
import Subtype

spec :: Spec
spec = 
  describe "STLC" $ do
    context "evaluating lambda" $
      it "should be true" $
        evaluate (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (Succ Zero)) ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"])) `shouldBe` 
        Just (Succ Zero)
    
    context "evaluating iszero" $
      it "should be true" $
        evaluate (IsZero (Pred (Succ Zero))) `shouldBe` Just (Tru)

    context "parsing conditional and arithmetics" $ 
      it "should be true" $ 
        parseExpr "if succ (succ 0) then true else false" `shouldBe` 
        Right (If (Succ (Succ Zero)) Tru Fls)

    context "printing lambda expression" $ 
      it "should be true" $ 
        printPretty (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) "y") ["y"]) `shouldBe` "\\ y : Nat->Nat . y"

    context "printing type error" $ 
      it "should be true" $ 
        printPretty (Mismatch Bool Nat) `shouldBe` "Type mismatch for function argument\n    got: Bool\n    but expected: Nat"

    context "printing type error" $ 
      it "should be true" $ 
        printPretty (NotRecord Tru) `shouldBe` "Couldn't perform projection on non-record expression: true"

    context "printing type" $ 
      it "should be true" $ 
        printPretty (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (Var 1 TUnit "z")) ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"])) 
        `shouldBe` "(\\ x : Nat->Nat . x z) (\\ y : Nat . y)"

    context "printing terms with records" $ 
      it "should be true" $ 
        printPretty (App (Lambda (TRec [("a",Nat),("b",TRec [("c",Bool)])]) (Var 0 (TRec [("a",Nat),("b",TRec[("c",Bool)])]) "x") ["x"]) (Rec [("a",Succ Zero),("b",Rec [("c",Tru),("d",Zero)]),("e",Fls)])) 
        `shouldBe` "(\\ x : {a:Nat, b:{c:Bool}} . x) {a=succ 0, b={c=true, d=0}, e=false}"

    context "parsing application" $ 
      it "should be true" $ 
        parseExpr "(\\ x : Nat->Nat . x z) (\\ y : Nat . y)" `shouldBe` 
        Right (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (Var 1 TUnit "z")) ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"]))

    context "parsing lambda" $ 
      it "should be true" $ 
        parseExpr "(\\ x : Nat.\\ y : Nat . x y)" `shouldBe` 
        Right (Lambda Nat (Lambda Nat (App (Var 1 Nat "x") (Var 0 Nat "y")) ["y"]) ["x","y"])

    context "parsing lambda expression" $ 
      it "should be true" $ 
        parseExpr "(\\x: Nat. \\ y : Nat . iszero 0)" `shouldBe` 
        Right (Lambda Nat (Lambda Nat (IsZero Zero) ["y"]) ["x","y"])

    context "parsing application" $ 
      it "should be true" $ 
        parseExpr "(\\x : Nat->Nat . x) (\\y : Nat . y)" `shouldBe` 
        Right (App (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) "x") ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"]))

    context "typechecking function" $ 
      it "should be true" $ 
        typeOf (Lambda Nat (Lambda Nat (IsZero Zero) ["y"]) ["x","y"]) `shouldBe` 
        Right (Arr Nat (Arr Nat Bool))

    context "typechecking function" $ 
      it "should be true" $ 
        typeOf (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) "x") ["x"]) `shouldBe` 
        Right (Arr (Arr Nat Nat) (Arr Nat Nat))

    context "typechecking application" $ 
      it "should be true" $ 
        typeOf (App (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) "x") ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"])) `shouldBe` 
        Right (Arr Nat Nat)

    context "typechecking application with subtyping" $ 
      it "should be true" $ 
        typeOf (App (Lambda (TRec [("a",Nat),("b",TRec [("c",Bool)])]) (Var 0 (TRec [("a",Nat),("b",TRec[("c",Bool)])]) "x") ["x"]) (Rec [("a",Succ Zero),("b",Rec [("c",Tru),("d",Zero)]),("e",Fls)])) `shouldBe` 
        Right (TRec [("a",Nat),("b",TRec [("c",Bool)])])

    context "actual type is not of param type" $ 
      it "should be true" $ 
        typeOf (App (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) "x") ["x"]) (Lambda Bool (Var 0 Bool "y") ["y"])) `shouldBe` 
        Left (Mismatch (Arr Bool Bool) (Arr Nat Nat))

    context "two arms of the conditional do not have the same type" $ 
      it "should be true" $ 
        typeOf (If Tru Tru Zero) `shouldBe` 
        Left (Difference Bool Nat)

    context "evaluating records" $ 
      it "should be true" $ 
        evaluate (Rec [("x", If Tru (IsZero Zero) Fls)]) `shouldBe` Just (Rec [("x",Tru)])

    context "evaluating record projection" $ 
      it "should be true" $ 
        evaluate (Proj (Rec [("x", If Tru (IsZero Zero) Fls)]) "x") `shouldBe` Just Tru

    context "parsing record" $ 
      it "should be true" $ 
        parseExpr "{x  =true, y=false}" `shouldBe` Right (Rec [("x",Tru),("y",Fls)])

    context "parsing projection" $ 
      it "should be true" $ 
        parseExpr "{x  =true, y=false}.x" `shouldBe` Right (Proj (Rec [("x",Tru),("y",Fls)]) "x")

    context "typechecking records" $ 
      it "should be true" $ 
        typeOf (Rec [("x", Succ Zero), ("y", Tru)]) `shouldBe` Right (TRec [("x",Nat),("y",Bool)])

    context "typechecking record projection" $ 
      it "should be true" $ 
        typeOf (Proj (Rec [("x", If Tru (IsZero Zero) Fls), ("y", Succ Zero)]) "x") `shouldBe` Right Bool

    context "subtyping records" $ 
      it "should be true" $ 
        isSubtype (TRec [("x", TRec [("c",Nat),("b",Nat)]),("y",Nat)]) (TRec [("x", TRec [("a",Nat)])]) 
        `shouldBe` False

    context "sort record fields" $ 
      it "should be true" $ 
        sortFields [("b",Nat),("a",Nat)]
        `shouldBe` [("a",Nat),("b",Nat)]

    context "subtyping records" $ 
      it "should be true" $ 
        isSubtype (TRec [("x", Nat), ("a", Bool), ("b", Bool)]) (TRec [("x", Nat), ("y", Nat)]) 
        `shouldBe` False

    context "subtyping records" $ 
      it "should be true" $ 
        isSubtype (TRec [("a", Bool), ("y", Nat), ("x", Nat)]) (TRec [("x", Nat), ("y", Nat)]) 
        `shouldBe` True

    context "subtyping records" $ 
      it "should be true" $ 
        isSubtype (TRec [("z", Bool)]) (TRec [("z", Bool)])
        `shouldBe` True 
               
    context "subtyping records" $ 
      it "should be true" $ 
        isSubtype (Arr (TRec [("y",Nat)]) (TRec [("z", Bool)])) (Arr (TRec [("x",Nat),("y",Nat)]) (TRec [("z", Bool)]))
        `shouldBe` True

    context "parsing record type" $ 
      it "should be true" $ 
        parseExpr "\\x: {a:Nat,b:{c:Bool}} . x"
        `shouldBe` Right (Lambda (TRec [("a",Nat),("b",TRec [("c",Bool)])]) (Var 0 (TRec [("a",Nat),("b",TRec [("c",Bool)])]) "x") ["x"])

    context "parsing record type" $ 
      it "should be true" $ 
        parseExpr "(\\x: {a:Nat,b:{c:Bool}} . x) {a=succ 0, b={c=true,d=0}, e=false}"
        `shouldBe` Right (App (Lambda (TRec [("a",Nat),("b",TRec [("c",Bool)])]) (Var 0 (TRec [("a",Nat),("b",TRec[("c",Bool)])]) "x") ["x"]) (Rec [("a",Succ Zero),("b",Rec [("c",Tru),("d",Zero)]),("e",Fls)]))

main :: IO ()
main = hspec spec