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
                ( S.fromList [ "a1a2", "a1b1", "a1b2"])
        it "returns correct coordinates for king in upper right corner" $ do
            S.fromList (possibleMoves whiteKing (strToCoordinate "h8")) 
                `shouldBe` 
                    ( S.fromList 
                        [ "h8g8"
                        , "h8g7"
                        , "h8h7"
                        ]
                    )
        it "returns correct coordinates for king in center" $ do
            S.fromList (possibleMoves whiteKing (strToCoordinate "d4")) 
                `shouldBe` 
                    ( S.fromList 
                        [ "d4d3"
                        , "d4c3"
                        , "d4c4"
                        , "d4c5"
                        , "d4d5"
                        , "d4e5"
                        , "d4e4"
                        , "d4e3"
                        ]
                    )
        it "returns correct coordinates for Queen in center" $ do
            S.fromList (possibleMoves whiteQueen (strToCoordinate "d4"))
                `shouldBe` 
                    ( S.fromList
                        [ "d4d1"
                        , "d4d2"
                        , "d4d3"
                        , "d4d5"
                        , "d4d6"
                        , "d4d7"
                        , "d4d8"
                        , "d4a4"
                        , "d4b4"
                        , "d4c4"
                        , "d4e4"
                        , "d4f4"
                        , "d4g4"
                        , "d4h4"
                        , "d4a1"
                        , "d4b2"
                        , "d4c3"
                        , "d4e5"
                        , "d4f6" 
                        , "d4g7"
                        , "d4h8"
                        , "d4c5"
                        , "d4b6"
                        , "d4a7"
                        , "d4e3"
                        , "d4f2"
                        , "d4g1"
                        ]
                    )
        it "returns correct coordinates for Rook in center" $ do
            S.fromList ( possibleMoves whiteRook (strToCoordinate "d4"))
                `shouldBe` 
                    ( S.fromList
                        [ "d4d1"
                        , "d4d2"
                        , "d4d3"
                        , "d4d5"
                        , "d4d6"
                        , "d4d7"
                        , "d4d8"
                        , "d4a4"
                        , "d4b4"
                        , "d4c4"
                        , "d4e4"
                        , "d4f4"
                        , "d4g4"
                        , "d4h4"
                        ]
                    )
        it "returns correct coordinates for bishop in center" $ do
            S.fromList (possibleMoves whiteBishop (strToCoordinate "d4"))
                `shouldBe` 
                    ( S.fromList 
                        [ "d4a1"
                        , "d4b2"
                        , "d4c3"
                        , "d4e5"
                        , "d4f6" 
                        , "d4g7"
                        , "d4h8"
                        , "d4c5"
                        , "d4b6"
                        , "d4a7"
                        , "d4e3"
                        , "d4f2"
                        , "d4g1"
                        ]
                    )
        it "returns correct coordinates for knight in lower right corner" $ do
            S.fromList (possibleMoves whiteKnight (strToCoordinate "a1"))
                `shouldBe` 
                    ( S.fromList 
                        [ "a1b3"
                        , "a1c2"
                        ]
                    )
        it "returns correct coordinates for knight in center" $ do
            S.fromList (possibleMoves whiteKnight (strToCoordinate "d4"))
                `shouldBe` 
                    ( S.fromList 
                        [ "d4b3"
                        , "d4c2"
                        , "d4b5"
                        , "d4c6"
                        , "d4e6"
                        , "d4f5"
                        , "d4f3"
                        , "d4e2"
                        ]
                    )
        it "returns correct coordinates for white pawn in center" $ do
            S.fromList (possibleMoves whitePawn (strToCoordinate "d4"))
                `shouldBe` 
                    ( S.fromList 
                        [ "d4d5"
                        , "d4c5"
                        , "d4e5"
                        ]
                    )
        it "returns correct coordinates for white pawn in start position" $ do
            S.fromList (possibleMoves whitePawn (strToCoordinate "d2"))
                `shouldBe` 
                    ( S.fromList 
                        [ "d2d3"
                        , "d2d4"
                        , "d2c3"
                        , "d2e3"
                        ]
                    )
        it "returns correct coordinates for white pawn about to promote" $ do
            S.fromList (possibleMoves whitePawn (strToCoordinate "d7"))
                `shouldBe` 
                    ( S.fromList 
                        [ "d7d8q"
                        , "d7d8r"
                        , "d7d8b"
                        , "d7d8n"
                        , "d7e8q"
                        , "d7e8r"
                        , "d7e8b"
                        , "d7e8n"
                        , "d7c8q"
                        , "d7c8r"
                        , "d7c8b"
                        , "d7c8n"
                        ]
                    )
        it "returns correct coordinates for black pawn in center" $ do
            S.fromList (possibleMoves blackPawn (strToCoordinate "d4"))
                `shouldBe` 
                    ( S.fromList 
                        [ "d4d3"
                        , "d4c3"
                        , "d4e3"
                        ]
                    )
        it "returns correct coordinates for black pawn in start position" $ do
            S.fromList (possibleMoves blackPawn (strToCoordinate "d7"))
                `shouldBe` 
                    ( S.fromList 
                        [ "d7d6"
                        , "d7d5"
                        , "d7c6"
                        , "d7e6"
                        ]
                    )
        it "returns correct coordinates for black pawn about to promote" $ do
            S.fromList (possibleMoves blackPawn (strToCoordinate "d2"))
                `shouldBe` 
                    ( S.fromList 
                        [ "d2d1q"
                        , "d2d1r"
                        , "d2d1b"
                        , "d2d1n"
                        , "d2e1q"
                        , "d2e1r"
                        , "d2e1b"
                        , "d2e1n"
                        , "d2c1q"
                        , "d2c1r"
                        , "d2c1b"
                        , "d2c1n"
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

blackPawn :: C.Piece
blackPawn = C.Piece C.Black C.Pawn
