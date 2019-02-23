module Rules
    ( 
    ) where

import qualified Board as B
import           Board (Game, Move, Coordinate)
import           Pieces (Piece(..), PieceType(..), Color(..))

validMoves :: Game -> Coordinate -> [Move]
validMoves g c = let b = B.board g
                 in case B.getPieceAt b c of
                         Piece King _ -> kingValidMoves g c 
                         Piece Queen _ -> queenValidMoves g c
                         Piece Rook _ -> rookValidMoves g c
                         Piece Bishop _ -> bishopValidMoves g c
                         Piece Knight _ -> knightValidMoves g c
                         Piece Pawn _ -> pawnValidMoves g c
                         NoPiece -> []

kingValidMoves :: Game -> Coordinate -> [Move]
kingValidMoves = undefined

queenValidMoves :: Game -> Coordinate -> [Move]
queenValidMoves = undefined

rookValidMoves :: Game -> Coordinate -> [Move]
rookValidMoves = undefined

bishopValidMoves :: Game -> Coordinate -> [Move]
bishopValidMoves = undefined

knightValidMoves :: Game -> Coordinate -> [Move]
knightValidMoves = undefined

pawnValidMoves :: Game -> Coordinate -> [Move]
pawnValidMoves = undefined
