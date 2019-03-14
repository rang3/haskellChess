module Main where

import           Interactive ( interactiveMain )
import           Uci ( uciMain )
import qualified Options as O

import qualified Chess as C
import qualified Chess.FEN as Fen
import qualified Control.Monad.State.Lazy as S
import           Data.List( nub )
import           Data.Maybe ( Maybe (Just, Nothing) )
import           System.Console.GetOpt( getOpt
                                      , usageInfo
                                      , ArgOrder( Permute ) 
                                      , OptDescr( Option )
                                      , ArgDescr( NoArg
                                                , OptArg
                                                , ReqArg
                                                )
                                      )
import           System.Environment ( getArgs )
import           System.Exit ( ExitCode (ExitFailure, ExitSuccess) 
                             , exitWith 
                             )
import           System.IO ( hPutStrLn
                           , Handle
                           , stderr
                           , stdin
                           )

main :: IO ()
main = do
    argv <- getArgs
    parse argv
    
parse argv = case getOpt Permute O.options argv of

    (actions,nonOptions,[]) -> do
        opts <- foldl (>>=) (return O.startOptions) actions
        if O.optInteractiveColor opts == Nothing
            then uciMain opts
            else interactiveMain opts

    (_,_,errors)  -> do
        hPutStrLn stderr (concat errors ++ usageInfo header O.options)
        exitWith (ExitFailure 1)

    where header = "Usage: haskellChess-exe [-ih] "
