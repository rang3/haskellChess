module GameTree 
    ( GameTree (..)
    , gameTreeFromBoard
    , minimax
    , minimaxAB
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

minimax :: GameTree -> Int -> (String,Float)
minimax gt 0 = ("", value gt)
minimax gt d = case color of
    C.White -> maxi gt d
    C.Black -> mini gt d
    where color = C.turn $ position gt

maxi :: GameTree -> Int -> (String,Float)
maxi gt 0 = minimax gt 0
maxi gt d = case subTrees gt of
    []  -> minimax gt 0
    gts -> argmax (\(_,v) -> v) $ 
            map (\(m, gt) -> (m, snd $ mini gt (d-1))) gts

mini :: GameTree -> Int -> (String,Float)
mini gt 0 = minimax gt 0
mini gt d = case subTrees gt of
    []  -> minimax gt 0
    gts -> argmin (\(_,v) -> v) $ 
            map (\(m, gt) -> (m, snd $ maxi gt (d-1))) gts

minimaxAB :: GameTree -> Int -> (String, Float)
minimaxAB gt 0 = ("", value gt)
minimaxAB gt d = case color of
    C.White -> maxiAB gt d (-1.0) 1.0
    C.Black -> miniAB gt d (-1.0) 1.0
    where color = C.turn $ position gt

maxiAB :: GameTree -> Int -> Float -> Float -> (String, Float)
maxiAB gt 0 a b = minimaxAB gt 0
maxiAB gt d a b = case subTrees gt of
    []   -> minimaxAB gt 0
    mgts -> pruneMaxiAB mgts d a b ("", (-1.0))

miniAB :: GameTree -> Int -> Float -> Float -> (String, Float)
miniAB gt 0 a b = minimaxAB gt 0
miniAB gt d a b = case subTrees gt of
    []   -> minimaxAB gt 0
    mgts -> pruneMiniAB mgts d a b ("", 1.0)

pruneMaxiAB :: [(String,GameTree)] -> Int -> Float -> Float -> (String, Float) 
    -> (String, Float)
pruneMaxiAB [] d a b (move, val) = (move,val)
pruneMaxiAB (mgt@(m,gt):mgts) d a b (move, val) =
    if a >= b
        then (move, val)
        else pruneMaxiAB mgts d a' b (move'', val'')
    where (move', val') = miniAB gt (d-1) a b
          (move'',val'') = if val' > val then (m, val') else (move, val)
          a' = max a val'

pruneMiniAB :: [(String,GameTree)] -> Int -> Float -> Float -> (String, Float) 
    -> (String, Float)
pruneMiniAB [] d a b (move, val) = (move,val)
pruneMiniAB (mgt@(m,gt):mgts) d a b (move, val) =
    if a >= b
        then (move, val)
        else pruneMiniAB mgts d a b' (move'', val'')
    where (move', val') = maxiAB gt (d-1) a b
          (move'',val'') = if val' < val then (m, val') else (move, val)
          b' = min b val'
