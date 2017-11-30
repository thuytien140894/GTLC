module EvalSpec where

import Test.Hspec
import Syntax
import Parser
import Eval
import Pretty
import Types
import TypeChecker

spec :: Spec
spec = 
  describe "STLC" $ do
    context "evaluation" $
      it "should be true" $
        eval (App (Lambda (Arr Nat Nat) (App (Var 0 "x") (Succ Zero)) [("x",Arr Nat Nat)]) (Lambda Nat (Var 0 "y") [("y",Nat)])) `shouldBe` 
        Just (Succ Zero)

    context "evaluation" $
      it "should be true" $
        eval (If (IsZero Zero) (Lambda Nat (Var 0 "x") [("x",Nat)]) (Var 0 "y")) `shouldBe` 
        Just (Lambda Nat (Var 0 "x") [("x",Nat)])
    
    context "evaluation" $
      it "should be true" $
        eval (IsZero (Pred (Succ Zero))) `shouldBe` Just (Tru)

    context "parsing" $ 
      it "should be true" $ 
        parseExpr "if succ (succ 0) then true else false" `shouldBe` 
        Right (If (Succ (Succ Zero)) Tru Fls)

    context "printing" $ 
      it "should be true" $ 
        printPretty (Lambda (Arr Nat Nat) (Var 0 "y") [("y",Nat)]) `shouldBe` "\\ y : Nat->Nat . y"

    context "binding indices" $ 
      it "should be true" $ 
        parseExpr "(\\ x : Nat->Nat . x z) (\\ y : Nat . y)" `shouldBe` 
        Right (App (Lambda (Arr Nat Nat) (App (Var 0 "x") (Var 1 "z")) [("x",Arr Nat Nat)]) (Lambda Nat (Var 0 "y") [("y",Nat)]))

    context "binding indices" $ 
      it "should be true" $ 
        parseExpr "(\\ x : Nat . \\ y : Nat . x y)" `shouldBe` 
        Right (Lambda Nat (Lambda Nat (App (Var 1 "x") (Var 0 "y")) [("x",Nat),("y",Nat)]) [("x",Nat),("y",Nat)])

    context "binding indices" $ 
      it "should be true" $ 
        parseExpr "(\\ x : Nat . \\ y : Nat . iszero 0)" `shouldBe` 
        Right (Lambda Nat (Lambda Nat (IsZero Zero) [("x",Nat),("y",Nat)]) [("x",Nat),("y",Nat)])

    context "binding indices" $ 
      it "should be true" $ 
        parseExpr "(\\ x : Nat->Nat . x) (\\ y : Nat . y)" `shouldBe` 
        Right (App (Lambda (Arr Nat Nat) (Var 0 "x") [("x",Arr Nat Nat)]) (Lambda Nat (Var 0 "y") [("y",Nat)]))

    context "type checking" $ 
      it "should be true" $ 
        typeOf (Lambda Nat (Lambda Nat (IsZero Zero) [("x",Nat),("y",Nat)]) [("x",Nat),("y",Nat)]) [] `shouldBe` 
        Just (Arr Nat (Arr Nat Bool))

    context "type checking" $ 
      it "should be true" $ 
        typeOf (Lambda (Arr Nat Nat) (Var 0 "x") [("x",Arr Nat Nat)]) [] `shouldBe` 
        Just (Arr (Arr Nat Nat) (Arr Nat Nat))

    context "type checking" $ 
      it "should be true" $ 
        typeOf (App (Lambda (Arr Nat Nat) (Var 0 "x") [("x",Arr Nat Nat)]) (Lambda Nat (Var 0 "y") [("y",Nat)])) [] `shouldBe` 
        Just (Arr Nat Nat)

main :: IO ()
main = hspec spec