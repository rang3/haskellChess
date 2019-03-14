module Main where

import           Interactive ( interactiveMain )
import           Uci ( uciMain )

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
import qualified Text.Read as R

main :: IO ()
main = do
    argv <- getArgs
    parse argv
    
parse argv = case getOpt Permute options argv of

    (actions,nonOptions,[]) -> do
        opts <- foldl (>>=) (return startOptions) actions
        if optInteractiveColor opts == Nothing
            then uciMain
            else interactiveMain

    (_,_,errors)  -> do
        hPutStrLn stderr (concat errors ++ usageInfo header options)
        exitWith (ExitFailure 1)

    where header = "Usage: cat [-benstuv] [file ...]"

data Options = Options { optInteractiveColor :: Maybe C.Color
                       }

startOptions :: Options
startOptions = Options { optInteractiveColor = Nothing
                       }

options = 
    [ Option ['i'] ["interactive"] 
        ( ReqArg 
            ( \arg opt -> case arg of
                            "white" -> return Options { optInteractiveColor 
                                = Just C.White }
                            "black" -> return Options { optInteractiveColor 
                                = Just C.Black }
                            _       -> do hPutStrLn stderr "not a valid color"
                                          exitWith $ ExitFailure 1
            )
            "COLOR"
        )
        "Allows the user to input a file."
    , Option ['h'] ["help"] 
        ( NoArg 
            ( \_ -> do hPutStrLn stderr (usageInfo "haskellChess" options)
                       exitWith ExitSuccess
            )
        )
        "print help"
    ]
