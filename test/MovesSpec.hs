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
            S.fromList (possibleMoves whiteKing (strToCoordinate "a1")) `shouldBe` 
                ( S.fromList $ map (strToCoordinate) [ "a2", "b1", "b2"])
        it "returns correct coordinates for king in upper right corner" $ do
            S.fromList (possibleMoves whiteKing (strToCoordinate "h8")) 
                `shouldBe` 
                    ( S.fromList $ map strToCoordinate 
                        [ "g8"
                        , "g7"
                        , "h7"
                        ]
                    )
        it "returns correct coordinates for king in center" $ do
            S.fromList (possibleMoves whiteKing (strToCoordinate "d4")) 
                `shouldBe` 
                    ( S.fromList $ map strToCoordinate 
                        [ "d3"
                        , "c3"
                        , "c4"
                        , "c5"
                        , "d5"
                        , "e5"
                        , "e4"
                        , "e3"
                        ]
                    )
        it "returns correct coordinates for Queen in center" $ do
            S.fromList (possibleMoves whiteQueen (strToCoordinate "d4"))
                `shouldBe` 
                    ( S.fromList $ map strToCoordinate
                        [ "d1"
                        , "d2"
                        , "d3"
                        , "d5"
                        , "d6"
                        , "d7"
                        , "d8"
                        , "a4"
                        , "b4"
                        , "c4"
                        , "e4"
                        , "f4"
                        , "g4"
                        , "h4"
                        , "a1"
                        , "b2"
                        , "c3"
                        , "e5"
                        , "f6" 
                        , "g7"
                        , "h8"
                        , "c5"
                        , "b6"
                        , "a7"
                        , "e3"
                        , "f2"
                        , "g1"
                        ]
                    )
        it "returns correct coordinates for Rook in center" $ do
            S.fromList ( possibleMoves whiteRook (strToCoordinate "d4"))
                `shouldBe` 
                    ( S.fromList $ map strToCoordinate
                        [ "d1"
                        , "d2"
                        , "d3"
                        , "d5"
                        , "d6"
                        , "d7"
                        , "d8"
                        , "a4"
                        , "b4"
                        , "c4"
                        , "e4"
                        , "f4"
                        , "g4"
                        , "h4"
                        ]
                    )
        it "returns correct coordinates for bishop in center" $ do
            S.fromList (possibleMoves whiteBishop (strToCoordinate "d4"))
                `shouldBe` 
                    ( S.fromList $ map strToCoordinate
                        [ "a1"
                        , "b2"
                        , "c3"
                        , "e5"
                        , "f6" 
                        , "g7"
                        , "h8"
                        , "c5"
                        , "b6"
                        , "a7"
                        , "e3"
                        , "f2"
                        , "g1"
                        ]
                    )
        it "returns correct coordinates for knight in lower right corner" $ do
            S.fromList (possibleMoves whiteKnight (strToCoordinate "a1"))
                `shouldBe` 
                    ( S.fromList $ map strToCoordinate
                        [ "b3"
                        , "c2"
                        ]
                    )
        it "returns correct coordinates for knight in center" $ do
            S.fromList (possibleMoves whiteKnight (strToCoordinate "d4"))
                `shouldBe` 
                    ( S.fromList $ map strToCoordinate
                        [ "b3"
                        , "c2"
                        , "b5"
                        , "c6"
                        , "e6"
                        , "f5"
                        , "f3"
                        , "e2"
                        ]
                    )
        it "returns correct coordinates for pawn in center" $ do
            S.fromList (possibleMoves whitePawn (strToCoordinate "d4"))
                `shouldBe` 
                    ( S.fromList $ map strToCoordinate
                        [ "d5"
                        , "d6"
                        , "c5"
                        , "e5"
                        , "d3"
                        , "d2"
                        , "c3"
                        , "e3"
                        ]
                    )

whiteKing :: C.Piece
whiteKing = C.Piece C.White C.King

whiteQueen :: C.Piece
whiteQueen = C.Piece C.White C.Queen

whiteRook :: C.Piece
whiteRook = C.Piece C.White C.Rook

whiteBishop :: C.Piece
whiteBishop = C.Piece C.White C.Bishop

whiteKnight :: C.Piece
whiteKnight = C.Piece C.White C.Knight

whitePawn :: C.Piece
whitePawn = C.Piece C.White C.Pawn
