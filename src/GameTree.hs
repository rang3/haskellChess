module GameTree 
    ( GameTree (..)
    ) where

import           Moves ( nextGameStates )
import           Eval ( eval )

import qualified Chess as C

data GameTree = GameTree { state :: C.Board
                         , nextStates :: [(String, C.Board)]
                         , score :: Float
                         }

gameTreeFromBoard :: C.Board -> GameTree
gameTreeFromBoard b = GameTree { state = b
                               , nextStates = nextGameStates b
                               , score = eval b
                               }

bestMove :: GameTree -> String
bestMove = undefined
