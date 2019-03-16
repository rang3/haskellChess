module MovesSpec where

import           Moves

import qualified Chess as C
import qualified Data.Set as S
import           Test.Hspec

-- sample failing test
-- TODO: add real tests
spec :: Spec
spec = do
    describe "possibleMoves" $ do
        it "returns correct coordinates for king in lower left corner" $ do
            S.fromList (possibleMoves whiteKing (Coordinate 0 0)) 
                `shouldBe` S.fromList [ Coordinate 0 1
                                      , Coordinate 1 0
                                      , Coordinate 1 1
                                      ]
        it "returns correct coordinates for king in upper right corner" $ do
            S.fromList (possibleMoves whiteKing (Coordinate 7 7)) 
                `shouldBe` S.fromList [ Coordinate 6 7
                                      , Coordinate 7 6
                                      , Coordinate 6 6
                                      ]

whiteKing :: C.Piece
whiteKing = C.Piece C.White C.King

