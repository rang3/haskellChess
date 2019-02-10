module Board
    ( Board(..)
    , Move(..)
    , Coordinate(..)
    , getPieceAt
    , initialGame
    , initialBoard
    , getPieceCoordinates
    ) where

import Pieces

type Board = [[Piece]]

data Game = StandardGame 
    { board         :: Board
    , turn          :: Color
    , moveHistory   :: [Move]
    }
    deriving (Eq, Show)

data Move = Move Coordinate Coordinate
    deriving (Eq, Show)

data Coordinate = Coordinate Int Int
    deriving (Eq, Show)

bp = Piece Pawn Black
bn = Piece Knight Black
bb = Piece Bishop Black
br = Piece Rook Black
bq = Piece Queen Black
bk = Piece King Black
wp = Piece Pawn White
wn = Piece Knight White
wb = Piece Bishop White
wr = Piece Rook White
wq = Piece Queen White
wk = Piece King White
na = NoPiece

initialBoard :: Board
initialBoard = [[br,bn,bb,bq,bk,bb,bn,br]
               ,[bp,bp,bp,bp,bp,bp,bp,bp]
               ,[na,na,na,na,na,na,na,na]
               ,[na,na,na,na,na,na,na,na]
               ,[na,na,na,na,na,na,na,na]
               ,[na,na,na,na,na,na,na,na]
               ,[wp,wp,wp,wp,wp,wp,wp,wp]
               ,[wr,wn,wb,wq,wk,wb,wn,wr]]

initialGame :: Game
initialGame = StandardGame 
    { board       = initialBoard
    , turn        = White
    , moveHistory = []
    }

getPieceCoordinates :: Board -> Color -> [Coordinate]
getPieceCoordinates b c = aux b c 0 0 []
    where aux b c 8 7 cs = cs
          aux b c f r cs | f == 8 = aux b c 0 (r+1) cs
                         | otherwise = 
                         case getPieceAt b (Coordinate f r) of  
                           NoPiece ->
                             aux b c (f+1) r cs
                           Piece _ pc -> 
                             if pc == c 
                             then 
                               aux b c (f+1) r ((Coordinate f r):cs)
                             else
                                aux b c (f+1) r cs

getPieceAt :: Board -> Coordinate -> Piece
getPieceAt b (Coordinate f r) = 
    (b !! (7 - r)) !! f

--TODO
--implement show for pieces
--implement monad for grid and board container
--tests
