module Rules
    ( 
    ) where

import qualified Board
import Pieces

validMoves :: Board.Game -> Board.Coordinate -> [Board.Move]
validMoves g c = let b = Board.board g
                 in case Board.getPieceAt b c of
                         Piece King _ -> kingValidMoves g c 
                         Piece Queen _ -> queenValidMoves g c
                         Piece Rook _ -> rookValidMoves g c
                         Piece Bishop _ -> bishopValidMoves g c
                         Piece Knight _ -> knightValidMoves g c
                         Piece Pawn _ -> pawnValidMoves g c
                         NoPiece -> []

kingValidMoves :: Board.Game -> Board.Coordinate -> [Board.Move]
kingValidMoves = undefined

queenValidMoves :: Board.Game -> Board.Coordinate -> [Board.Move]
queenValidMoves = undefined

rookValidMoves :: Board.Game -> Board.Coordinate -> [Board.Move]
rookValidMoves = undefined

bishopValidMoves :: Board.Game -> Board.Coordinate -> [Board.Move]
bishopValidMoves = undefined

knightValidMoves :: Board.Game -> Board.Coordinate -> [Board.Move]
knightValidMoves = undefined

pawnValidMoves :: Board.Game -> Board.Coordinate -> [Board.Move]
pawnValidMoves = undefined
