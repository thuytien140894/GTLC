module EvalSpec where

import SpecHelper
import Syntax

spec :: Spec
spec = 
  describe "STLC" $ do
    context "evaluation" $
      it "should be true" $
        eval (App (Lambda (Var 0 "x") ["x"]) (Lambda (Var 0 "y") ["y"])) `shouldBe` Just (Lambda (Var 0 "y") ["y"])

    context "evaluation" $
      it "should be true" $
        eval (If Tru (Lambda (Var 0 "x") ["x"]) (Var 0 "y")) `shouldBe` Just (Lambda (Var 0 "x") ["x"])
    
    context "evaluation" $
      it "should be true" $
        eval (IsZero (Pred (Succ Zero))) `shouldBe` Just (Tru)

    context "parsing" $ 
      it "should be true" $ 
        parseExpr "iszero (pred (succ 0))" `shouldBe` 
        Right (IsZero (Pred (Succ Zero)))

main :: IO ()
main = hspec spec