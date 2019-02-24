module Game
    ( Game(..)
    , executeMove
    , initialGame
    ) where

import qualified Board as B
import           Board (Board, Move, Coordinate)
import qualified Pieces as P
import           Pieces (Color(..))

import           Control.Monad.State.Lazy (State, get, put, execState)

data Game = StandardGame
    { board                 :: Board
    , turn                  :: Color
    , castlingAvailability  :: CastlingAvailability
    , enPassantTarget       :: Maybe Coordinate
    , halfmoveClock         :: Int
    , fullmoveNumber        :: Int
    } deriving (Eq, Show)

data CastlingAvailability = CastlingAvailability
    { whiteKingside  :: Bool
    , whiteQueenside :: Bool
    , blackKingside  :: Bool
    , blackQueenside :: Bool
    } deriving (Eq)

instance Show CastlingAvailability where
    show c = concat 
        [ ((\wk -> if wk then "K" else "") $ whiteKingside c)
        , ((\wq -> if wq then "Q" else "") $ whiteKingside c)
        , ((\bk -> if bk then "k" else "") $ whiteKingside c)
        , ((\bq -> if bq then "q" else "") $ whiteKingside c)
        ]
        

-- TODO: implement castling, enpassant, halfmoveclock, fullmovenumber here
executeMove :: Move -> State Game ()
executeMove m = do currentState <- get
                   let currentBoard = board currentState
                   let nextBoard = execState (B.movePiece m) currentBoard
                   put currentState { turn = P.switchColor $ turn currentState
                                    , board = execState (B.movePiece m) $ board currentState
                                    }

initialGame :: Game
initialGame = StandardGame
    { board                = B.initialBoard
    , turn                 = White
    , castlingAvailability = CastlingAvailability True True True True
    , enPassantTarget      = Nothing
    , halfmoveClock        = 0
    , fullmoveNumber       = 0
    }

