module EvalSpec where

import Test.Hspec
import Syntax
import Parser
import Evaluator
import Prettier
import Types
import TypeErrors
import TypeChecker

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
        printPretty (Difference Tru Zero) `shouldBe` "true and 0 do not have the same type."

    context "printing type" $ 
      it "should be true" $ 
        printPretty (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (Var 1 TUnit "z")) ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"])) 
        `shouldBe` "(\\ x : Nat->Nat . x z) (\\ y : Nat . y)"

    context "parsing application" $ 
      it "should be true" $ 
        parseExpr "(\\ x : Nat->Nat . x z) (\\ y : Nat . y)" `shouldBe` 
        Right (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (Var 1 TUnit "z")) ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"]))

    context "parsing lambda" $ 
      it "should be true" $ 
        parseExpr "(\\ x : Nat . \\ y : Nat . x y)" `shouldBe` 
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

    context "actual type is not of param type" $ 
      it "should be true" $ 
        typeOf (App (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) "x") ["x"]) (Lambda Bool (Var 0 Bool "y") ["y"])) `shouldBe` 
        Left (Mismatch (Lambda Bool (Var 0 Bool "y") ["y"]) (Arr Nat Nat))

    context "two arms of the conditional do not have the same type" $ 
      it "should be true" $ 
        typeOf (If Tru Tru Zero) `shouldBe` 
        Left (Difference Tru Zero)

    context "evaluating records" $ 
      it "should be true" $ 
        evaluate (Rec [("x", If Tru (IsZero Zero) Fls)]) `shouldBe` Just (Rec [("x",Tru)])

    context "evaluating record projection" $ 
      it "should be true" $ 
        evaluate (Proj (Rec [("x", If Tru (IsZero Zero) Fls)]) "x") `shouldBe` Just Tru

    context "parsing record" $ 
      it "should be true" $ 
        parseExpr "{x  =true, y=false}" `shouldBe` Right (Rec [("x",Tru),("y",Fls)])

    context "typechecking records" $ 
      it "should be true" $ 
        typeOf (Rec [("x", Succ Zero), ("y", Tru)]) `shouldBe` Right (TRec [("x",Nat),("y",Bool)])

    context "typechecking record projection" $ 
      it "should be true" $ 
        typeOf (Proj (Rec [("x", If Tru (IsZero Zero) Fls), ("y", Succ Zero)]) "x") `shouldBe` Right Bool

main :: IO ()
main = hspec spec