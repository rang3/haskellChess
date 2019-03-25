module GameTree 
    ( GameTree (..)
    , gameTreeFromBoard
    , minimax
    ) where

import           Moves ( nextGameStates )
import           Eval ( eval )

import qualified Chess as C
import           Data.List.Extras.Argmax ( argmax, argmin )

data GameTree = GameTree { position :: C.Board
                         , subTrees :: [(String, GameTree)]
                         , value :: Float
                         }

instance Show GameTree where
    show gt = show (position gt) ++ "\n" ++ show (value gt)

gameTreeFromBoard :: C.Board -> GameTree
gameTreeFromBoard b = GameTree { position = b
                               , subTrees = getSubTrees b
                               , value = eval b
                               }
    where getSubTrees b = map (\(m, nb) -> (m, gameTreeFromBoard nb))
                              (nextGameStates b)

--negamax :: GameTree -> Int -> (String, Float)
--negamax gt 0 = ("",value gt)
--negamax gt d = case subTrees gt of 
--    [] -> negamax gt 0
--    _  -> argmax (\(_,v) -> v) $ 
--            map (\(m,gt) -> (m, -1 * (snd $ negamax gt (d-1)))) (subTrees gt)

minimax :: GameTree -> Int -> (String,Float)
minimax gt 0 = ("", value gt)
minimax gt d = case color of
    C.White -> maxi gt d
    C.Black -> mini gt d
    where color = C.turn $ position gt

maxi :: GameTree -> Int -> (String,Float)
maxi gt 0 = minimax gt 0
maxi gt d = case subTrees gt of
    [] -> minimax gt 0
    _  -> argmax (\(_,v) -> v) $ 
            map (\(m, gt) -> (m, snd $ mini gt (d-1))) (subTrees gt)

mini :: GameTree -> Int -> (String,Float)
mini gt 0 = minimax gt 0
mini gt d = case subTrees gt of
    [] -> minimax gt 0
    _  -> argmin (\(_,v) -> v) $ 
            map (\(m, gt) -> (m, snd $ maxi gt (d-1))) (subTrees gt)
