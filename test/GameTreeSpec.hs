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
        it "calculates initial moves in a timely manner" $ do
            minimax middleGame 2 `shouldNotBe` ("", -1.0)
    
    describe "minimax with ab pruning" $ do
        it "mates in 1" $ do
            minimaxAB mateIn1 1 `shouldBe` ("f6g7",1.0) 
        it "sameResult as minimax" $ do
            minimaxAB middleGame 1 `shouldBe` (minimax middleGame 1)
        it "calculates initial moves in a timely manner" $ do
            minimaxAB middleGame 4 `shouldBe` ("", -1.0)

mateIn1 :: GameTree
mateIn1 = gameTreeFromBoard $ fromJust $ FEN.fromFEN
    "8/7k/p4Q2/P5R1/8/3P1b1P/P4P1P/5RK1 w - - 1 38"

initialGame :: GameTree
initialGame = gameTreeFromBoard $ fromJust $ FEN.fromFEN
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

middleGame :: GameTree
middleGame = gameTreeFromBoard $ fromJust $ FEN.fromFEN
    "r1bq1rk1/ppp2ppp/2np1n2/2b1p1B1/2B1P3/2PP1N2/PP3PPP/RN1Q1RK1 b - -"
