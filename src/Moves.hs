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

possibleMoves :: C.Piece -> Coordinate -> [String]
possibleMoves piece coordinate = case piece of 
    C.Piece _       C.King -> possibleMovesForKing coordinate
    C.Piece _       C.Queen -> possibleMovesForQueen coordinate
    C.Piece _       C.Rook -> possibleMovesForRook coordinate
    C.Piece _       C.Bishop -> possibleMovesForBishop coordinate
    C.Piece _       C.Knight -> possibleMovesForKnight coordinate
    C.Piece C.White C.Pawn -> possibleMovesForWhitePawn coordinate
    C.Piece C.Black C.Pawn -> possibleMovesForBlackPawn coordinate

possibleMovesForKing :: Coordinate -> [String]
possibleMovesForKing c@(Coordinate f r) = tail
    [show c ++ show (Coordinate f' r') 
        | f' <- [f,f+1,f-1], r' <- [r, r+1, r-1], validSquare f' r']

possibleMovesForQueen :: Coordinate -> [String]
possibleMovesForQueen c1 = possibleMovesForRook c1 ++ possibleMovesForBishop c1

possibleMovesForRook :: Coordinate -> [String]
possibleMovesForRook c@(Coordinate f r ) = 
    [show c ++ show (Coordinate f r') | r' <- [0..7], r' /= r]
    ++ [show c ++ show (Coordinate f' r) | f' <- [0..7], f' /= f]

possibleMovesForBishop :: Coordinate -> [String]
possibleMovesForBishop c@(Coordinate f r) = 
    [show c ++ show (Coordinate (f+x) (r+x)) 
        | x <- [-7.. -1]++[1..7], validSquare (f+x) (r+x)]
    ++ [show c ++ show (Coordinate (f+x) (r-x)) 
        | x <- [-7.. -1]++[1..7], validSquare (f+x) (r-x)]

possibleMovesForKnight :: Coordinate -> [String]
possibleMovesForKnight c@(Coordinate f r) = 
    [show c ++ show (Coordinate f' r') 
        | f' <- [f+1, f-1], r' <- [r+2, r-2], validSquare f' r']
    ++ [show c ++ show (Coordinate f' r') 
        | f' <- [f+2, f-2], r' <- [r+1, r-1], validSquare f' r']

-- pawn promotion is not handled here
possibleMovesForWhitePawn :: Coordinate -> [String]
possibleMovesForWhitePawn c@(Coordinate f r) = case r of
    1 -> (show c ++ show (Coordinate f (r+2))):movesNoJump -- starting jump
    6 -> [m ++ p | m <- movesNoJump, p <- ["q","r","b","n"]] -- promotion
    _ -> movesNoJump
    where movesNoJump = [show c ++ show (Coordinate f' (r+1))
                            | f' <- [f, f+1, f-1]]

possibleMovesForBlackPawn :: Coordinate -> [String]
possibleMovesForBlackPawn c@(Coordinate f r) = case r of
    1 -> [m ++ p | m <- movesNoJump, p <- ["q","r","b","n"]] -- promotion
    6 -> (show c ++ show (Coordinate f (r-2))):movesNoJump -- starting jump
    _ -> movesNoJump
    where movesNoJump = [show c ++ show (Coordinate f' (r-1))
                            | f' <- [f, f+1, f-1]]

validSquare :: Int -> Int -> Bool
validSquare f r = f >= 0 && f < 8 && r >= 0 && r < 8
