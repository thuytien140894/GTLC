module PrintSpec where 

    import Test.Hspec
    import Types
    import Prettier
    import TypeErrors

    spec :: Spec
    spec =
        describe "STLC" $ do
            context "printing type error" $ 
                it "should be true" $ 
                    printPretty (Mismatch Bool Nat) `shouldBe` "Type mismatch for function argument\n    got: Bool\n    but expected: Nat"

    -- main :: IO ()
    -- main = hspec spec