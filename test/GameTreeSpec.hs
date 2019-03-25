module GameTreeSpec where

import           GameTree

import qualified Chess as C
import qualified Chess.FEN as FEN
import           Data.Maybe ( fromJust )
import           Test.Hspec

spec :: Spec
spec = do
    describe "minimax" $ do
        it "mates in 1" $ do
            minimax mateIn1 1 `shouldBe` ("f6g7",1.0) 

mateIn1 :: GameTree
mateIn1 = gameTreeFromBoard $ fromJust $ FEN.fromFEN
    "8/7k/p4Q2/P5R1/8/3P1b1P/P4P1P/5RK1 w - - 1 38"
