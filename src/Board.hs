module Board
    ( Board(..)
    , Move(..)
    , Coordinate(..)
    , getPieceAt
    , initialGame
    , initialBoard
    , getPieceCoordinates
    , move
    ) where

import Pieces
import Control.Monad.State.Lazy

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
getPieceAt (Board g) (Coordinate f r) = 
    (g !! (7 - r)) !! f

setPiece :: Board -> Piece -> Coordinate -> Board
setPiece (Board g) p (Coordinate f r) =
    Board $ editList g (7-r) row
        where row = editList (g !! (7-r)) f p
    
editList :: [a] -> Int -> a -> [a]
editList xs idx elem = take idx xs ++ [elem] ++ drop (idx + 1) xs


move :: Move -> State Game ()
move m = do currentState <- get
            let oldMoveHistory = moveHistory currentState
            let currentBoard = board currentState
            let nextBoard = execState (movePiece m) currentBoard
            put currentState {    moveHistory = (m : oldMoveHistory)
                                , turn = switchColor $ turn currentState
                                , board = execState (movePiece m) $ board currentState
                             }

movePiece :: Move -> State Board ()
movePiece (Move c1 c2) = 
    do currBoard <- get
       let currPiece = getPieceAt currBoard c1
       setSquare c1 na
       setSquare c2 currPiece

setSquare :: Coordinate -> Piece -> State Board ()
setSquare c p =
    do board <- get
       put $ setPiece board p c


getLegalMoves :: Board -> Color -> [Move]
getLegalMoves = undefined

getAttackedSquares :: Board -> Color -> [Coordinate]
getAttackedSquares = undefined

--TODO
--implement show for pieces
--use vector or array for grid representation
--implement monad for board and game container
--tests
