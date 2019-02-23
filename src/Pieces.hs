module Pieces
    ( Piece(..)
    , Color(..)
    , PieceType(..)
    , switchColor
    ) where

import qualified Data.Char as Char

data Color = White | Black
    deriving (Eq, Show)

data Piece = Piece PieceType Color | NoPiece
    deriving Eq

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn
    deriving Eq

instance Show PieceType where
    show King = "K"
    show Queen = "Q"
    show Rook = "R"
    show Bishop = "B"
    show Knight = "N"
    show Pawn = "P"

instance Show Piece where
    show NoPiece = "."
    show (Piece t White) = show t
    show (Piece t Black) = fmap Char.toLower $ show t

switchColor :: Color -> Color
switchColor White = Black
switchColor Black = White
