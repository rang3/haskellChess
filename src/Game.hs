module Game
    ( Game(..)
    , executeMove
    , initialGame
    ) where

import qualified Board as B
import           Board (Board, Move)
import qualified Pieces as P
import           Pieces (Color(..))

import           Control.Monad.State.Lazy (State, get, put, execState)

data Game = StandardGame
    { board         :: Board
    , turn          :: Color
    , moveHistory   :: [Move]
    }
    deriving (Eq, Show)

executeMove :: Move -> State Game ()
executeMove m = do currentState <- get
                   let oldMoveHistory = moveHistory currentState
                   let currentBoard = board currentState
                   let nextBoard = execState (B.movePiece m) currentBoard
                   put currentState {    moveHistory = (m : oldMoveHistory)
                                       , turn = P.switchColor $ turn currentState
                                       , board = execState (B.movePiece m) $ board currentState
                                    }

initialGame :: Game
initialGame = StandardGame
    { board       = B.initialBoard
    , turn        = White
    , moveHistory = []
    }

