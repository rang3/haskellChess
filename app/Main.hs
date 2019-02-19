module Main where

import Board
import MoveParser
import Text.Read
import Control.Monad.State.Lazy

main :: IO ()
main = do
    loop initialGame

loop :: Game -> IO ()
loop game = do
    putStrLn $ show game
    move <- getMove 
    let game' = execState (executeMove move) game
    loop game'

getMove :: IO Move
getMove = do
    putStrLn "move:" 
    c <- getLine
    let mc = parseMove c
    case mc of
        Nothing -> getMove
        Just rc -> return rc

