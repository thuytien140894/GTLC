module EvalSpec where

import Test.Hspec
import Syntax
import Parser
import Eval
import Pretty
import Types

spec :: Spec
spec = 
  describe "STLC" $ do
    context "evaluation" $
      it "should be true" $
        eval (App (Lambda Nat (Var 0 "x") ["x"]) (Lambda Nat (Var 0 "y") ["y"])) `shouldBe` 
        Just (Lambda Nat (Var 0 "y") ["y"])

    context "evaluation" $
      it "should be true" $
        eval (If (IsZero Zero) (Lambda Nat (Var 0 "x") ["x"]) (Var 0 "y")) `shouldBe` 
        Just (Lambda Nat (Var 0 "x") ["x"])
    
    context "evaluation" $
      it "should be true" $
        eval (IsZero (Pred (Succ Zero))) `shouldBe` Just (Tru)

    context "parsing" $ 
      it "should be true" $ 
        parseExpr "\\ x : Nat->Nat->(Nat->Nat) . x" `shouldBe` 
        Right (Lambda (Arr (Arr Nat Nat) (Arr Nat Nat)) (Var 0 "x") ["x"])

    context "printing" $ 
      it "should be true" $ 
        printPretty (Succ (Succ Zero)) `shouldBe` "succ (succ 0)"

main :: IO ()
main = hspec spec