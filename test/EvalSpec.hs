module EvalSpec where

import SpecHelper
import Syntax

spec :: Spec
spec = 
  describe "shift" $ do
    context "term shifting" $
      it "should be true" $
        shift 0 2 (Lambda (Lambda (App (Var 1) (App (Var 0) (Var 2))))) `shouldBe` 
        Lambda (Lambda (App (Var 1) (App (Var 0) (Var 4))))

    context "substitution" $
      it "should be true" $
        subs 0 (Var 1) (Lambda (App (Var 1) (Var 0))) `shouldBe` Lambda (App (Var 2) (Var 0))

    context "evaluation" $
      it "should be true" $
        eval (App (Lambda (Var 0)) (Lambda (Var 1))) `shouldBe` Just (Lambda (Var 1))

    context "evaluation" $
      it "should be true" $
        eval (App (Lambda (Lambda (Var 1))) (Lambda (Var 0))) `shouldBe` Just (Lambda (Lambda (Var 0)))

    context "multi-step evaluation" $
      it "should be true" $
        eval (App (App (Lambda (Lambda (App (Var 1) (Var 0)))) (Lambda (Var 0))) (Lambda (Var 1))) `shouldBe` 
        Just (Lambda (Var 1))

    context "parsing" $
      it "should be true" $
        parseExpr "(\\ x . \\ y . x y) true false" `shouldBe` 
        Right (App (App (Lambda (Lambda (App (Var 0) (Var 0)))) Tr) Fl)

main :: IO ()
main = hspec spec