module MovesSpec where

import Moves

import Test.Hspec

-- sample failing test
-- TODO: add real tests
spec :: Spec
spec = do
    describe "possibleMoves" $ do
        it "returns 1" $ do
            nextGameStates `shouldBe` 1

