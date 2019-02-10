module Pieces
    ( Piece(..)
    , Color(..)
    , PieceType(..)
    ) where

data Color = White | Black
    deriving (Eq, Show)

data Piece = Piece PieceType Color | NoPiece
    deriving (Eq, Show)

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn
    deriving (Eq, Show)
