module Main where

import qualified Board as B
import           Board (Game, Move)
import qualified MoveParser as P

import qualified Text.Read as R
import qualified Control.Monad.State.Lazy as S

main :: IO ()
main = do
    loop B.initialGame

loop :: Game -> IO ()
loop game = do
    putStrLn $ show game
    move <- getMove 
    let game' = S.execState (B.executeMove move) game
    loop game'

getMove :: IO Move
getMove = do
    putStrLn "move:" 
    c <- getLine
    let mc = P.parseMove c
    case mc of
        Nothing -> getMove
        Just rc -> return rc

