module Rules
    ( isCheckmate
    , isValidMove
    ) where

import qualified Board

validMoves :: Board.Game -> Board.Coordinate -> [Board.Move]
validMoves g c = let b = board g
                 in case Board.getPieceAt c b of
                         Piece King _ -> kingValidMoves g c 
                         Piece Queen _ -> queenValidMoves g c
                         Piece Rook _ -> rookValidMoves g c
                         Piece Bishop _ -> bishopValidMoves g c
                         Piece Knight _ -> knightValidMoves g c
                         Piece Pawn _ -> pawnValidMoves g c
                         NoPiece -> []

kingValidMoves :: Board.Game -> Board.Coordinate -> [Board.Move]
kingValidMoves = undefined
