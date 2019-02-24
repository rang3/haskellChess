module Board
    ( Board(..)
    , Move(..)
    , Coordinate(..)
    , initialBoard
    , movePiece
    , putPiece
    , getPiece
    , pieceAt
    ) where

import qualified Pieces as P
import           Pieces (Color(..), Piece(..), PieceType(..))

import qualified Control.Monad.State.Lazy as S
import           Control.Monad.State.Lazy (State)
import           Data.Char (ord, chr)

newtype Board = Board { grid :: [[Piece]] }
    deriving Eq

instance Show Board where
    show b = '\n' : (concat $ fmap showRow $ grid b)
        where showRow [] = "\n"
              showRow (p:r) = show p ++ showRow r

data Move = Move Coordinate Coordinate
    deriving (Eq)

instance Show Move where
  show (Move x y) = (show x) ++ "-" ++ (show y)

data Coordinate = Coordinate Int Int
    deriving (Eq)

instance Show Coordinate where
  show (Coordinate x y) = let row = chr(ord 'a'+x)
                              rank = show (y+1)
                          in row:rank

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

initialGrid = [[br,bn,bb,bq,bk,bb,bn,br]
              ,[bp,bp,bp,bp,bp,bp,bp,bp]
              ,[na,na,na,na,na,na,na,na]
              ,[na,na,na,na,na,na,na,na]
              ,[na,na,na,na,na,na,na,na]
              ,[na,na,na,na,na,na,na,na]
              ,[wp,wp,wp,wp,wp,wp,wp,wp]
              ,[wr,wn,wb,wq,wk,wb,wn,wr]]

initialBoard :: Board
initialBoard = Board initialGrid

pieceAt :: Board -> Coordinate -> Piece
pieceAt (Board g) (Coordinate f r) =
    (g !! (7 - r)) !! f

updateBoard :: Board -> Piece -> Coordinate -> Board
updateBoard (Board g) p (Coordinate f r) =
    Board $ editList g (7-r) row
        where row = editList (g !! (7-r)) f p

editList :: [a] -> Int -> a -> [a]
editList xs idx elem = take idx xs ++ [elem] ++ drop (idx + 1) xs

movePiece :: Move -> State Board ()
movePiece (Move c1 c2) =
    do currBoard <- S.get
       let currPiece = pieceAt currBoard c1
       S.when (currPiece /= na) $ do putPiece c1 na
                                     putPiece c2 currPiece

putPiece :: Coordinate -> Piece -> State Board ()
putPiece c p =
    do board <- S.get
       S.put $ updateBoard board p c

getPiece :: Coordinate -> State Board Piece
getPiece c =
    do board <- S.get
       return $ pieceAt board c
