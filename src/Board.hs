module Board
    ( Board(..)
    , Move(..)
    , Game(..)
    , Coordinate(..)
    , getPieceAt
    , initialGame
    , initialBoard
    , executeMove
    ) where

import qualified Pieces as P
import           Pieces (Color(..), Piece(..), PieceType(..))

import           Control.Monad.State.Lazy (State, get, put, execState)
import           Data.Char (ord, chr)

newtype Board = Board { grid :: [[Piece]] }
    deriving Eq

instance Show Board where
    show b = '\n' : (concat $ fmap showRow $ grid b)
        where showRow [] = "\n"
              showRow (p:r) = show p ++ showRow r

data Game = StandardGame
    { board         :: Board
    , turn          :: Color
    , moveHistory   :: [Move]
    }
    deriving (Eq, Show)

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

initialGame :: Game
initialGame = StandardGame
    { board       = initialBoard
    , turn        = White
    , moveHistory = []
    }

--edit for array
getPieceAt :: Board -> Coordinate -> Piece
getPieceAt (Board g) (Coordinate f r) =
    (g !! (7 - r)) !! f

--edit for array
setPiece :: Board -> Piece -> Coordinate -> Board
setPiece (Board g) p (Coordinate f r) =
    Board $ editList g (7-r) row
        where row = editList (g !! (7-r)) f p

--remove for array
editList :: [a] -> Int -> a -> [a]
editList xs idx elem = take idx xs ++ [elem] ++ drop (idx + 1) xs


executeMove :: Move -> State Game ()
executeMove m = do currentState <- get
                   let oldMoveHistory = moveHistory currentState
                   let currentBoard = board currentState
                   let nextBoard = execState (movePiece m) currentBoard
                   put currentState {    moveHistory = (m : oldMoveHistory)
                                       , turn = P.switchColor $ turn currentState
                                       , board = execState (movePiece m) $ board currentState
                                    }

--edit for array
movePiece :: Move -> State Board ()
movePiece (Move c1 c2) =
    do currBoard <- get
       let currPiece = getPieceAt currBoard c1
       setSquare c1 na
       setSquare c2 currPiece

--edit for array
setSquare :: Coordinate -> Piece -> State Board ()
setSquare c p =
    do board <- get
       put $ setPiece board p c

--TODO
--use vector or array for grid representation
