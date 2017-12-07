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
    context "evaluation" $
      it "should be true" $
        evaluate (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (Succ Zero)) ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"])) `shouldBe` 
        Just (Succ Zero)
    
    context "evaluation" $
      it "should be true" $
        evaluate (IsZero (Pred (Succ Zero))) `shouldBe` Just (Tru)

    context "parsing" $ 
      it "should be true" $ 
        parseExpr "if succ (succ 0) then true else false" `shouldBe` 
        Right (If (Succ (Succ Zero)) Tru Fls)

    context "printing" $ 
      it "should be true" $ 
        printPretty (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) "y") ["y"]) `shouldBe` "\\ y : Nat->Nat . y"

    context "printing" $ 
      it "should be true" $ 
        printPretty (Difference Tru Zero) `shouldBe` "true and 0 do not have the same type."

    context "printing" $ 
      it "should be true" $ 
        printPretty (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (Var 1 TUnit "z")) ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"])) 
        `shouldBe` "(\\ x : Nat->Nat . x z) (\\ y : Nat . y)"

    context "binding indices" $ 
      it "should be true" $ 
        parseExpr "(\\ x : Nat->Nat . x z) (\\ y : Nat . y)" `shouldBe` 
        Right (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (Var 1 TUnit "z")) ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"]))

    context "binding indices" $ 
      it "should be true" $ 
        parseExpr "(\\ x : Nat . \\ y : Nat . x y)" `shouldBe` 
        Right (Lambda Nat (Lambda Nat (App (Var 1 Nat "x") (Var 0 Nat "y")) ["y"]) ["x","y"])

    context "binding indices" $ 
      it "should be true" $ 
        parseExpr "(\\x: Nat. \\ y : Nat . iszero 0)" `shouldBe` 
        Right (Lambda Nat (Lambda Nat (IsZero Zero) ["y"]) ["x","y"])

    context "binding indices" $ 
      it "should be true" $ 
        parseExpr "(\\x : Nat->Nat . x) (\\y : Nat . y)" `shouldBe` 
        Right (App (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) "x") ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"]))

    context "type checking" $ 
      it "should be true" $ 
        typeOf (Lambda Nat (Lambda Nat (IsZero Zero) ["y"]) ["x","y"]) `shouldBe` 
        Right (Arr Nat (Arr Nat Bool))

    context "type checking" $ 
      it "should be true" $ 
        typeOf (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) "x") ["x"]) `shouldBe` 
        Right (Arr (Arr Nat Nat) (Arr Nat Nat))

    context "type checking" $ 
      it "should be true" $ 
        typeOf (App (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) "x") ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"])) `shouldBe` 
        Right (Arr Nat Nat)

    context "type checking" $ 
      it "should be true" $ 
        typeOf (App (Lambda (Arr Nat Nat) (Var 0 (Arr Nat Nat) "x") ["x"]) (Lambda Bool (Var 0 Bool "y") ["y"])) `shouldBe` 
        Left (Mismatch (Lambda Bool (Var 0 Bool "y") ["y"]) (Arr Nat Nat))

    context "type checking" $ 
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

main :: IO ()
main = hspec spec