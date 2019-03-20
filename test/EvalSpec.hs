module EvalSpec where

import Eval

import qualified Chess as C
import qualified Chess.FEN as FEN
import           Data.Maybe ( fromJust )
import           Test.Hspec

spec :: Spec
spec = do
    describe "eval" $ do
        it "returns 1 when white wins" $ do
            eval whiteWins `shouldBe` (1 :: Float)

whiteWins :: C.Board
whiteWins = fromJust $ FEN.fromFEN
    "8/6Qk/p7/P5R1/8/3P1b1P/P4P1P/5RK1 b - - 1 38"
