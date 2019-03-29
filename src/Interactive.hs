module Interactive
    ( interactiveMain
    ) where

import qualified GameTree as GT
import           Options ( Options )

import qualified Chess as C
import qualified Chess.FEN as FEN
import           System.Exit ( ExitCode (ExitFailure, ExitSuccess) 
                             , exitWith 
                             )

interactiveMain :: Options -> IO ()
interactiveMain opts = do 
    putStrLn "interactive mode"
    board <- getStartingBoard
    let gt = GT.gameTreeFromBoard board
    gameLoop gt
    exitWith ExitSuccess

getStartingBoard :: IO C.Board
getStartingBoard = do
    putStrLn "put the FEN notation of the game position that you want to start from"
    fen <- getLine
    let board = FEN.fromFEN fen
    case board of
        Just b  -> return b
        Nothing -> getStartingBoard
                     
gameLoop :: GT.GameTree -> IO ()
gameLoop gt = do
    putStrLn $ show gt
    putStrLn "enter move:"
    move <- getLine
    let nextTree = lookup move $ GT.subTrees gt
    case nextTree of
        Just ngt -> do putStrLn $ show ngt
                       d <- getSearchDepth
                       let (m,v) = GT.minimaxAB ngt d
                       let cpuMove = lookup m $ GT.subTrees ngt
                       case cpuMove of
                         Just cpugt -> do putStrLn m
                                          gameLoop cpugt
                         Nothing   -> do putStrLn "No moves to be made"
        Nothing  -> do putStrLn "Invalid Move"
                       gameLoop gt

getSearchDepth :: IO Int
getSearchDepth = do putStrLn $ "search depth:"
                    d <- getLine
                    return (read d :: Int)
