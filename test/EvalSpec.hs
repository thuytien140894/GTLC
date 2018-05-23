module EvalSpec where

    import Error
    import Evaluator
    import Parser
    import Subtype
    import Syntax
    import Type
    import TypeChecker

    import Test.Hspec

    spec :: Spec
    spec = 
        describe "evaluates" $ do
            context "(\\x:Nat->Nat. x (succ 0)) (\\y:Nat. y)" $
                it "should be succ 0" $
                    evaluate (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (Succ Zero)) ["x"]) (Lambda Nat (Var 0 Nat "y") ["y"])) 
                    `shouldBe` Right (Succ Zero)
            
            context "iszero (pred (succ 0))" $
                it "should be true" $
                    evaluate (IsZero (Pred (Succ Zero))) 
                    `shouldBe` Right Tru

            context "(\\x:Nat->Nat. (x 0)) (\\x:Nat. (succ x))" $ 
                it "should be succ 0" $ 
                    evaluate (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Lambda Nat (Succ (Var 0 Nat "x")) ["x"])) 
                    `shouldBe` Right (Succ Zero)

            context "(\\x:Nat->Boolean. (x (succ 0))) (\\x:Nat. (iszero x))" $ 
                it "should be false" $ 
                    evaluate (App (Lambda (Arr Nat Boolean) (App (Var 0 (Arr Nat Boolean) "x") (Succ Zero)) ["x"]) (Lambda Nat (IsZero (Var 0 Nat "x")) ["x"]))
                    `shouldBe` Right Fls

            context "(\\x. (<Func?>x <Nat!>0)) <Func!><Nat?->Nat!>(\\x:Nat. (succ x))" $
                it "should be succ 0" $
                    evaluate (App (Lambda Dyn (App (Cast (Project (Arr Dyn Dyn) (Label 0)) (Var 0 Dyn "x")) (Cast (Inject Nat) Zero)) ["x"]) (Cast (Seq (Func (Project Nat (Label 1)) (Inject Nat)) (Inject (Arr Dyn Dyn))) (Lambda Nat (Succ (Var 0 Nat "x")) ["x"])))
                    `shouldBe` Right (Succ Zero)

            context "(\\x. succ <Nat?>x) <Boolean!>true" $
                it "should be Fail 0" $
                    evaluate (App (Lambda Dyn (Succ (Cast (Project Nat (Label 0)) (Var 0 Dyn "x"))) ["x"]) (Cast (Inject Boolean) Tru))
                    `shouldBe` Left (CastError Boolean Nat (BlameRes None (Succ (Cast (Fail Boolean Nat (Label 0)) Tru))))

            context "(\\x:Nat->Nat. x (x 0)) <Nat!->Iden>(\\x. (succ x))" $
                it "should be succ (succ 0)" $
                    evaluate (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") (App (Var 0 (Arr Nat Nat) "x") Zero)) ["x"]) (Cast (Func (Inject Nat) (Iden Nat)) (Lambda Dyn (Succ (Cast (Project Nat (Label 0)) (Var 0 Dyn "x"))) ["x"])))
                    `shouldBe` Right (Succ (Succ Zero))

            context "(\\m. ((\\x:Nat->Nat. (x 0)) <Nat!->Nat?><Func?>m)) <Func!><Nat?->Nat!>(\\y:Nat. succ y)" $
                it "should be succ 0" $
                    evaluate (App (Lambda Dyn (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Cast (Seq (Project (Arr Dyn Dyn) (Label 0)) (Func (Inject Nat) (Project Nat (Label 1)))) (Var 0 Dyn "m"))) ["m","x"]) (Cast (Seq (Func (Project Nat (Label 2)) (Inject Nat)) (Inject (Arr Dyn Dyn))) (Lambda Nat (Succ (Var 0 Nat "y")) ["y"])))
                    `shouldBe` Right (Succ Zero)

            context "(\\m. ((\\x. (<Func?>x <Nat!>0)) m)) <Func!><Nat?->Nat!>(\\y:Nat. succ y)" $
                it "should be succ 0" $
                    evaluate (App (Lambda Dyn (App (Lambda Dyn (App (Cast (Project (Arr Dyn Dyn) (Label 0)) (Var 0 Dyn "x")) (Cast (Inject Nat) Zero)) ["x"]) (Var 0 Dyn "m")) ["m","x"]) (Cast (Seq (Func (Project Nat (Label 1)) (Inject Nat)) (Inject (Arr Dyn Dyn))) (Lambda Nat (Succ (Var 0 Nat "y")) ["y"])))
                    `shouldBe` Right (Succ Zero)

            context "(\\m. ((\\x:Nat->Nat. (x 0)) <Nat!->Nat?><Func?>m)) <Func!><Nat?->Boolean!>(\\y:Nat. iszero y)" $
                it "should be blame 1" $
                    evaluate (App (Lambda Dyn (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Cast (Seq (Project (Arr Dyn Dyn) (Label 0)) (Func (Inject Nat) (Project Nat (Label 1)))) (Var 0 Dyn "m"))) ["m","x"]) (Cast (Seq (Func (Project Nat (Label 2)) (Inject Boolean)) (Inject (Arr Dyn Dyn))) (Lambda Nat (IsZero (Var 0 Nat "y")) ["y"])))
                    `shouldBe` Left (CastError Boolean Nat (BlameRes FunRet (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Cast (Fail Boolean Nat (Label 1)) (Lambda Nat (IsZero (Var 0 Nat "y")) ["y"])))))

            context "(\\m. ((\\x:Nat->Boolean. (x 0)) <Nat!->Boolean?><Func?>m)) <Func!><Nat?->Nat!>(\\y:Nat. succ y)" $
                it "should be blame 1" $
                    evaluate (App (Lambda Dyn (App (Lambda (Arr Nat Boolean) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Cast (Seq (Project (Arr Dyn Dyn) (Label 0)) (Func (Inject Nat) (Project Boolean (Label 1)))) (Var 0 Dyn "m"))) ["m","x"]) (Cast (Seq (Func (Project Nat (Label 2)) (Inject Nat)) (Inject (Arr Dyn Dyn))) (Lambda Nat (Succ (Var 0 Nat "y")) ["y"])))
                    `shouldBe` Left (CastError Nat Boolean (BlameRes FunRet (App (Lambda (Arr Nat Boolean) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Cast (Fail Nat Boolean (Label 1)) (Lambda Nat (Succ (Var 0 Nat "y")) ["y"])))))

            context "(\\m. ((\\x:Nat->Nat. (x 0)) <Nat!->Nat?><Func?>m)) <Boolean!>true" $
                it "should be blame 0" $
                    evaluate (App (Lambda Dyn (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Cast (Seq (Project (Arr Dyn Dyn) (Label 0)) (Func (Inject Nat) (Project Nat (Label 1)))) (Var 0 Dyn "m"))) ["m","x"]) (Cast (Inject Boolean) Tru))
                    `shouldBe` Left (CastError Boolean (Arr Dyn Dyn) (BlameRes Function (App (Lambda (Arr Nat Nat) (App (Var 0 (Arr Nat Nat) "x") Zero) ["x"]) (Cast (Fail Boolean (Arr Dyn Dyn) (Label 0)) Tru))))

            context "if (\\x. iszero x) <Nat!>(succ 0) then <Nat!>0 else <Nat?>(\\x. <FuncProj>x <Nat!>0) <FuncInj><I->Nat!>(\\y. succ <Nat?>y)" $
                it "should be succ 0" $
                    evaluate (If (App (Lambda Dyn (IsZero (Cast (Project Nat (Label 0)) (Var 0 Dyn "x"))) ["x"]) (Cast (Inject Nat) (Succ Zero)))(Cast (Inject Nat) Zero) (Cast (Project Nat (Label 3)) (App (Lambda Dyn (App (Cast (Project (Arr Dyn Dyn) (Label 1)) (Var 0 Dyn "x")) (Cast(Inject Nat) Zero)) ["x"]) (Cast (Seq (Func (Iden Dyn) (Inject Nat)) (Inject (Arr Dyn Dyn))) (Lambda Dyn (Succ (Cast (Project Nat (Label 2)) (Var 0 Dyn "y"))) ["y"])))))
                    `shouldBe` Right (Succ Zero)

            context "((\\m. if (\\x. iszero <Nat?>x) m then (\\x. succ <Nat?>x) else (\\x. pred <Nat?>x)) <Nat!>0) <Boolean!>true" $
                it "should be blame 1" $
                    evaluate (App (App (Lambda Dyn (If (App (Lambda Dyn (IsZero (Cast (Project Nat (Label 0)) (Var 0 Dyn "x"))) ["x"]) (Var 0 Dyn"m")) (Lambda Dyn (Succ (Cast (Project Nat (Label 1)) (Var 0 Dyn "x"))) ["x"]) (Lambda Dyn (Pred (Cast (Project Nat (Label 2)) (Var 0 Dyn "x"))) ["x"])) ["m"]) (Cast (Inject Nat) Zero)) (Cast (Inject Boolean) Tru))
                    `shouldBe` Left (CastError Boolean Nat (BlameRes None (Succ (Cast (Fail Boolean Nat (Label 1)) Tru))))
            
            context "(\\n. (\\m. if (\\x. iszero <Nat?>x) n then <FuncProj>m <Nat!>0 else <FuncProj>m <Nat!>(succ 0)) <FuncInj><I->Nat!>(\\y. succ <Nat?>y)) <Nat!>succ 0" $
                it "should be succ (succ 0)" $
                    evaluate (App (Lambda Dyn (App (Lambda Dyn (If (App (Lambda Dyn (IsZero (Cast (Project Nat (Label 0)) (Var 0 Dyn "x"))) ["x"]) (Var 1 Dyn "n")) (App (Cast (Project (Arr Dyn Dyn) (Label 1)) (Var 0 Dyn "m")) (Cast (Inject Nat) Zero)) (App (Cast (Project (Arr Dyn Dyn)(Label 2)) (Var 0 Dyn "m")) (Cast (Inject Nat) (Succ Zero)))) ["m"]) (Cast (Seq (Func (Iden Dyn) (Inject Nat)) (Inject (Arr Dyn Dyn))) (Lambda Dyn (Succ (Cast (Project Nat (Label 3)) (Var 0 Dyn "y"))) ["y"]))) ["n","m","y"]) (Cast (Inject Nat) (Succ Zero)))
                    `shouldBe` Right (Succ (Succ Zero))

            context "(\\x. <Ref?>x:=<Nat!>succ 0) <Ref!><Ref Nat? Nat!>ref 0" $
                it "should be succ (succ 0)" $
                    evaluate (App (Lambda Dyn (Assign (Cast (Project (TRef Dyn) (Label 0)) (Var 0 Dyn "x")) (Cast (Inject Nat) (Succ Zero))) ["x"]) (Cast (Seq (CRef (Project Nat (Label 1)) (Inject Nat)) (Inject (TRef Dyn))) (Ref Zero)))
                    `shouldBe` Right (Succ Zero)

            context "(\\m. ((\\x. (<Fun?>x <Ref!><Ref Nat? Nat!>ref (succ (succ 0)))) m)) <Fun!><<Ref Nat! Nat?><Ref?>->Nat!>(\\y:Ref Nat. !y)" $
                it "should be succ (succ 0)" $
                    evaluate (App (Lambda Dyn (App (Lambda Dyn (App (Cast (Project (Arr Dyn Dyn) (Label 0)) (Var 0 Dyn "x")) (Cast (Seq (CRef (Project Nat (Label 1)) (Inject Nat)) (Inject (TRef Dyn))) (Ref (Succ (Succ Zero))))) ["x"]) (Var 0 Dyn "m")) ["m","x"]) (Cast (Seq (Func (Seq (Project (TRef Dyn) (Label 2)) (CRef (Inject Nat) (Project Nat (Label 3)))) (Inject Nat)) (Inject (Arr Dyn Dyn))) (Lambda (TRef Nat) (Deref (Var 0 (TRef Nat) "y")) ["y"])))
                    `shouldBe` Right (Succ (Succ Zero))

            context "(\\m. ((\\x. (<Fun?>x <Nat!>(succ (succ 0))) m)) <Fun!><<Ref Nat! Nat?><Ref?>->Nat!>(\\y:Ref Nat. !y)" $
                it "should be blame" $
                    evaluate (App (Lambda Dyn (App (Lambda Dyn (App (Cast (Project (Arr Dyn Dyn) (Label 0)) (Var 0 Dyn "x")) (Cast (Inject Nat) (Succ (Succ Zero)))) ["x"]) (Var 0 Dyn "m")) ["m","x"]) (Cast (Seq (Func (Seq (Project (TRef Dyn) (Label 1)) (CRef (Inject Nat) (Project Nat (Label 2)))) (Inject Nat)) (Inject (Arr Dyn Dyn))) (Lambda (TRef Nat) (Deref (Var 0 (TRef Nat) "y")) ["y"])))
                    `shouldBe` Left (CastError Nat (TRef Dyn) (BlameRes Reference (Cast (Inject Nat) (App (Lambda (TRef Nat) (Deref (Var 0 (TRef Nat) "y")) ["y"]) (Cast (Fail Nat (TRef Dyn) (Label 1)) (Succ (Succ Zero)))))))

            context "<Fail2->Nat!><Fail1->Nat?>(succ 0)" $
                it "should be Fail2" $
                    evaluate (Cast (Func (Fail Boolean Nat (Label 2)) (Inject Nat)) (Cast (Func (Fail Boolean Nat (Label 1)) (Project Nat (Label 1))) (Succ Zero)))
                    `shouldBe` Left (CastError Boolean Nat (BlameRes FunArg (Cast (Func (Fail Boolean Nat (Label 2)) (Inject Nat)) (Cast (Fail Boolean Nat (Label 1))(Succ Zero)))))

            context "<Fail2><Int?><Boolean!>true" $
                it "should be Fail 1" $
                    evaluate (Cast (Fail Boolean Nat (Label 2)) (Cast (Project Nat (Label 1)) (Cast (Inject Boolean) Tru)))
                    `shouldBe` Left (CastError Boolean Nat (BlameRes None (Cast (Fail Boolean Nat (Label 2)) (Cast (Fail Boolean Nat (Label 1)) Tru))))
            
            context "<Fail2><Int?><Boolean!>true" $
                it "should be Fail 1" $
                    evaluate (Cast (Fail Boolean Nat (Label 1)) (Cast (Func (Iden Boolean) (Project Nat (Label 2))) (Cast (Func (Iden Boolean) (Inject Boolean)) Tru)))
                    `shouldBe` Left (CastError Boolean Nat (BlameRes FunRet (Cast (Fail Boolean Nat (Label 1)) (Cast (Fail Boolean Nat (Label 2)) Tru))))

            context "<Fail2><Int?><Boolean!>true" $
                it "should be Fail 1" $
                    evaluate (Cast (Seq (Func (Iden Boolean) (Inject Boolean)) (Seq (Func (Iden Boolean) (Project Nat (Label 2))) (Fail Boolean Nat (Label 1)))) Tru)
                    `shouldBe` Left (CastError Boolean Nat (BlameRes FunRet (Cast (Fail Boolean Nat (Label 2)) Tru)))