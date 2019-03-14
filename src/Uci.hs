module Uci 
    ( uciMain
    ) where

import           Options ( Options )

import           System.Exit ( ExitCode (ExitFailure, ExitSuccess) 
                             , exitWith 
                             )

uciMain :: Options -> IO ()
uciMain opts = do 
    putStrLn "uci mode"
    exitWith ExitSuccess
