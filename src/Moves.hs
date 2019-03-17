module Moves
    ( Coordinate ( Coordinate )
    , Move ( Move ) 
    , nextGameStates
    , possibleMoves
    , strToCoordinate
    ) where

import qualified Chess as C
import           Data.Char (ord, chr)
import qualified Text.Read as R

data Move = Move Coordinate Coordinate
    deriving (Eq)

instance Show Move where
    show (Move x y) = (show x) ++ (show y)

data Coordinate = Coordinate Int Int
    deriving (Eq)

instance Show Coordinate where
    show (Coordinate x y) = let row = chr(ord 'a'+x)
                                rank = show (y+1)
                             in row:rank

instance Ord Coordinate where
    compare c1 c2 = compare (show c1) (show c2)

strToCoordinate :: String -> Coordinate
strToCoordinate s = let (f, r) = C.strToPos s
                     in Coordinate f r

nextGameStates = undefined

possibleMoves :: C.Piece -> Coordinate -> [Coordinate]
possibleMoves piece coordinate = case piece of 
    C.Piece _   C.King -> possibleMovesForKing coordinate
    C.Piece _   C.Queen -> possibleMovesForQueen coordinate
    C.Piece _   C.Rook -> possibleMovesForRook coordinate
    C.Piece _   C.Bishop -> possibleMovesForBishop coordinate
    C.Piece _   C.Knight -> possibleMovesForKnight coordinate
    C.Piece clr C.Pawn -> possibleMovesForPawn coordinate

possibleMovesForKing :: Coordinate -> [Coordinate]
possibleMovesForKing (Coordinate f r) = tail
    [Coordinate f' r' | f' <- [f,f+1,f-1], r' <- [r, r+1, r-1], validSquare f' r']

possibleMovesForQueen :: Coordinate -> [Coordinate]
possibleMovesForQueen = undefined

possibleMovesForRook :: Coordinate -> [Coordinate]
possibleMovesForRook = undefined

possibleMovesForBishop :: Coordinate -> [Coordinate]
possibleMovesForBishop = undefined

possibleMovesForKnight :: Coordinate -> [Coordinate]
possibleMovesForKnight = undefined

-- pawn promotion is not handled here
possibleMovesForPawn :: Coordinate -> [Coordinate]
possibleMovesForPawn = undefined

validSquare :: Int -> Int -> Bool
validSquare f r = f >= 0 && f < 8 && r >= 0 && r < 8
