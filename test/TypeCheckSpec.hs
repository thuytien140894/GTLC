module TypeCheckSpec where

  import Test.Hspec
  import TypeChecker
  import Parser
  import Errors
  import Types
  import Syntax

  spec :: Spec
  spec = 
    describe "casts" $ do
      context "(\\x. 0) True" $
        it "should be (\\x. 0) <Bool!>True" $ 
          typeCheck (App (Lambda Dyn Zero ["x"]) Tru) `shouldBe` 
          Right (App (Lambda Dyn Zero ["x"]) (Cast (Inject Bool) Tru))

      context "(\\x. (x 0)) (\\x:Nat. (succ x))" $
        it "should be (\\x. (<Fun?>x <Nat!>0)) <Fun!><Nat?->Nat!>(\\x:Nat. (succ x))" $ 
          typeCheck (App (Lambda Dyn (App (Var 0 Dyn "x") Zero) ["x"]) (Lambda Nat (Succ (Var 0 Nat "x")) ["x"])) `shouldBe` 
          Right (App (Lambda Dyn (App (Cast (Project (Arr Dyn Dyn) (Label 0)) (Var 0 Dyn "x")) (Cast (Inject Nat) Zero)) ["x"]) (Cast (Seq (Func (Project Nat (Label 1)) (Inject Nat)) (Inject (Arr Dyn Dyn))) (Lambda Nat (Succ (Var 0 Nat "x")) ["x"])))
          
      context "(\\x:Dyn->Nat. (x 0)) (\\x:Nat. x)" $
        it "should be (\\x:Dyn->Nat. (x <Nat!>0)) <Nat?->Identity>(\\x:Nat. x)" $ 
          typeCheck (App (Lambda (Arr Dyn Nat) (App (Var 0 (Arr Dyn Nat) "x") Zero) ["x"]) (Lambda Nat (Var 0 Nat "x") ["x"])) `shouldBe` 
          Right (App (Lambda (Arr Dyn Nat) (App (Var 0 (Arr Dyn Nat) "x") (Cast (Inject Nat) Zero)) ["x"]) (Cast (Func (Project Nat (Label 0)) (Iden Nat)) (Lambda Nat (Var 0 Nat "x") ["x"])))

      context "(\\x. succ x) true" $
        it "should be (\\x. succ <Nat?>x) <Bool!>true" $
          typeCheck (App (Lambda Dyn (Succ (Var 0 Dyn "x")) ["x"]) Tru) `shouldBe` 
          Right (App (Lambda Dyn (Succ (Cast (Project Nat (Label 0)) (Var 0 Dyn "x"))) ["x"]) (Cast (Inject Bool) Tru))

      context "(\\x:Bool->Nat. succ 0) (\\x:Nat. x)" $
        it "should be (\\x:Bool->Nat. succ <Identity>0) <Fail>(\\x:Nat. x)" $
          typeCheck (App (Lambda (Arr Bool Nat) (Succ Zero) ["x"]) (Lambda Nat (Var 0 Nat "x") ["x"])) `shouldBe` 
          Left (Mismatch (Arr Nat Nat) (Arr Bool Nat))

      context "(\\x:Bool. x) 0" $
        it "should be Mismatch" $
          typeCheck (App (Lambda Bool (Var 0 Bool "x") ["x"]) Zero)
          `shouldBe` Left (Mismatch Nat Bool)

      context "(\\m. ((\\x:Nat->Nat. (x 0)) m)) (\\y:Nat. succ y)" $
        it "should be (\\m. ((\\x:Nat->Nat. (x 0)) <Nat!->Nat?><Fun?>m)) <Fun!><Nat?->Nat!>(\\y:Nat. succ y)" $
          typeCheck (App (Lambda Dyn (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Var 0 Dyn "m")) ["m","x"]) (Lambda Nat (Succ (Var 0 Nat "y")) ["y"]))
          `shouldBe` Right (App (Lambda Dyn (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Cast (Seq (Project (Arr Dyn Dyn) (Label 0)) (Func (Inject Nat) (Project Nat (Label 1)))) (Var 0 Dyn "m"))) ["m","x"]) (Cast (Seq (Func (Project Nat (Label 2)) (Inject Nat)) (Inject (Arr Dyn Dyn))) (Lambda Nat (Succ (Var 0 Nat "y")) ["y"])))

      context "(\\m. ((\\x:Nat->Nat. (x 0)) m)) (\\y:Nat. iszero y)" $
        it "should be (\\m. ((\\x:Nat->Nat. (x 0)) <Nat!->Nat?><Fun?>m)) <Fun!><Nat?->Bool!>(\\y:Nat. iszero y)" $
          typeCheck (App (Lambda Dyn (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Var 0 Dyn "m")) ["m","x"]) (Lambda Nat (IsZero (Var 0 Nat "y")) ["y"]))
          `shouldBe` Right (App (Lambda Dyn (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Cast (Seq (Project (Arr Dyn Dyn) (Label 0)) (Func (Inject Nat) (Project Nat (Label 1)))) (Var 0 Dyn "m"))) ["m","x"]) (Cast (Seq (Func (Project Nat (Label 2)) (Inject Bool)) (Inject (Arr Dyn Dyn))) (Lambda Nat (IsZero (Var 0 Nat "y")) ["y"])))

      context "(\\m. ((\\x:Nat->Nat. (x 0)) m)) true" $
        it "should be (\\m. ((\\x:Nat->Nat. (x 0)) <Nat!->Nat?><Fun?>m)) <Bool!>true" $
          typeCheck (App (Lambda Dyn (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Var 0 Dyn "m")) ["m","x"]) Tru)
          `shouldBe` Right (App (Lambda Dyn (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Cast (Seq (Project (Arr Dyn Dyn) (Label 0)) (Func (Inject Nat) (Project Nat (Label 1)))) (Var 0 Dyn "m"))) ["m","x"]) (Cast (Inject Bool) Tru))
      
      context "(\\m. ((\\x. (x 0)) m)) (\\y:Nat. succ y)" $
        it "should be (\\m. ((\\x. (<Fun?>x <Nat!>0)) m)) <Fun!><Nat?->Nat!>(\\y:Nat. succ y)" $
          typeCheck (App (Lambda Dyn (App (Lambda Dyn (App (Var 0 Dyn "x") Zero) ["x"]) (Var 0 Dyn "m")) ["m","x"]) (Lambda Nat (Succ (Var 0 Nat "y")) ["y"]))
          `shouldBe` Right (App (Lambda Dyn (App (Lambda Dyn (App (Cast (Project (Arr Dyn Dyn) (Label 0)) (Var 0 Dyn "x")) (Cast (Inject Nat) Zero)) ["x"]) (Var 0 Dyn "m")) ["m","x"]) (Cast (Seq (Func (Project Nat (Label 1)) (Inject Nat)) (Inject (Arr Dyn Dyn))) (Lambda Nat (Succ (Var 0 Nat "y")) ["y"])))

      context "(\\x:Nat->Nat. x (x 0)) (\\x. (succ x))" $
        it "should be (\\x:Nat->Nat. x (x 0)) <Nat!->Iden>(\\x. (succ x))" $
          typeCheck (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (App (Var 0 (Arr Nat Nat) "x") Zero)) ["x"]) (Lambda Dyn (Succ (Var 0 Dyn "x")) ["x"]))
          `shouldBe` Right (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (App (Var 0 (Arr Nat Nat) "x") Zero)) ["x"]) (Cast (Func (Inject Nat) (Iden Nat)) (Lambda Dyn (Succ (Cast (Project Nat (Label 0)) (Var 0 Dyn "x"))) ["x"])))

      context "if (\\x. iszero x) (succ 0) then 0 else (\\x. x 0) (\\y. succ y)" $
        it "should be if (\\x. iszero x) <Nat!>(succ 0) then <Nat!>0 else <Nat?>(\\x. <Fun?>x <Nat!>0) <Fun!><I->Nat!>(\\y. succ <Nat?>y)" $
          typeCheck (If (App (Lambda Dyn (IsZero (Var 0 Dyn "x")) ["x"]) (Succ Zero)) Zero (App (Lambda Dyn (App (Var 0 Dyn "x") Zero) ["x"]) (Lambda Dyn (Succ (Var 0 Dyn "y")) ["y"])))
          `shouldBe` Right (If (App (Lambda Dyn (IsZero (Cast (Project Nat (Label 0)) (Var 0 Dyn "x"))) ["x"]) (Cast (Inject Nat) (Succ Zero)))(Cast (Inject Nat) Zero) (Cast (Project Nat (Label 3)) (App (Lambda Dyn (App (Cast (Project (Arr Dyn Dyn) (Label 1)) (Var 0 Dyn "x")) (Cast(Inject Nat) Zero)) ["x"]) (Cast (Seq (Func (Iden Dyn) (Inject Nat)) (Inject (Arr Dyn Dyn))) (Lambda Dyn (Succ (Cast (Project Nat (Label 2)) (Var 0 Dyn "y"))) ["y"])))))

      context "((\\m. if (\\x. iszero x) m then (\\x. succ x) else (\\x. pred x)) 0) true" $
        it "should be if ((\\m. if (\\x. iszero <Nat?>x) m then (\\x. succ <Nat?>x) else (\\x. pred <Nat?>x)) <Nat!>0) <Bool!>true" $
          typeCheck (App (App (Lambda Dyn (If (App (Lambda Dyn (IsZero (Var 0 Dyn "x")) ["x"]) (Var 0 Dyn "m")) (Lambda Dyn (Succ (Var 0Dyn "x")) ["x"]) (Lambda Dyn (Pred (Var 0 Dyn "x")) ["x"])) ["m"]) Zero) Tru)
          `shouldBe` Right (App (App (Lambda Dyn (If (App (Lambda Dyn (IsZero (Cast (Project Nat (Label 0)) (Var 0 Dyn "x"))) ["x"]) (Var 0 Dyn"m")) (Lambda Dyn (Succ (Cast (Project Nat (Label 1)) (Var 0 Dyn "x"))) ["x"]) (Lambda Dyn (Pred (Cast (Project Nat (Label 2)) (Var 0 Dyn "x"))) ["x"])) ["m"]) (Cast (Inject Nat) Zero)) (Cast (Inject Bool) Tru))

      context "(\\n. (\\m. if (\\x. iszero x) n then m 0 else m (succ 0)) (\\y. succ y)) 0" $
        it "should be (\\n. (\\m. if (\\x. iszero <Nat?>x) n then <Fun?>m <Nat!>0 else <Fun?>m <Nat!>(succ 0)) <Fun!><I->Nat!>(\\y. succ <Nat?>y)) <Nat!>0" $
          typeCheck (App (Lambda Dyn (App (Lambda Dyn (If (App (Lambda Dyn (IsZero (Var 0 Dyn "x")) ["x"]) (Var 1 Dyn "n")) (App (Var 0 Dyn "m") Zero) (App (Var 0 Dyn "m") (Succ Zero))) ["m"]) (Lambda Dyn (Succ (Var 0 Dyn "y")) ["y"])) ["n","m","y"]) Zero)
          `shouldBe` Right (App (Lambda Dyn (App (Lambda Dyn (If (App (Lambda Dyn (IsZero (Cast (Project Nat (Label 0)) (Var 0 Dyn "x"))) ["x"]) (Var 1 Dyn "n")) (App (Cast (Project (Arr Dyn Dyn) (Label 1)) (Var 0 Dyn "m")) (Cast (Inject Nat) Zero)) (App (Cast (Project (Arr Dyn Dyn)(Label 2)) (Var 0 Dyn "m")) (Cast (Inject Nat) (Succ Zero)))) ["m"]) (Cast (Seq (Func (Iden Dyn) (Inject Nat)) (Inject (Arr Dyn Dyn))) (Lambda Dyn (Succ (Cast (Project Nat (Label 3)) (Var 0 Dyn "y"))) ["y"]))) ["n","m","y"]) (Cast (Inject Nat) Zero))

      context "(\\x. x:=succ 0) ref 0" $
        it "should be (\\x. <Ref?>x:=<Nat!>succ 0) <Ref!><Ref Nat? Nat!>ref 0" $
          typeCheck (App (Lambda Dyn (Assign (Var 0 Dyn "x") (Succ Zero)) ["x"]) (Ref Zero))
          `shouldBe` Right (App (Lambda Dyn (Assign (Cast (Project (TRef Dyn) (Label 0)) (Var 0 Dyn "x")) (Cast (Inject Nat) (Succ Zero))) ["x"]) (Cast (Seq (CRef (Project Nat (Label 1)) (Inject Nat)) (Inject (TRef Dyn))) (Ref Zero)))