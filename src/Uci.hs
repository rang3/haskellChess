module Uci 
    ( uciMain
    ) where

import           System.Exit ( ExitCode (ExitFailure, ExitSuccess) 
                             , exitWith 
                             )

uciMain :: IO ()
uciMain = do putStrLn "uci mode"
             exitWith ExitSuccess
