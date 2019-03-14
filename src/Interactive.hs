module Interactive
    ( interactiveMain
    ) where

import           System.Exit ( ExitCode (ExitFailure, ExitSuccess) 
                             , exitWith 
                             )

interactiveMain :: IO ()
interactiveMain = do putStrLn "interactive mode"
                     exitWith ExitSuccess
                     
-- TODO: rewrite the gameloop using the hackage chess library
--loop :: Game -> IO ()
--loop game = do
--    putStrLn $ show game
--    move <- getMove 
--    let game' = S.execState (G.executeMove move) game
--    loop game'
--
--getMove :: IO Move
--getMove = do
--    putStrLn "move:" 
--    c <- getLine
--    let mc = P.parseMove c
--    case mc of
--        Nothing -> getMove
--        Just rc -> return rc
