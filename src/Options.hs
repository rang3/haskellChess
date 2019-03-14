module Options
    ( Options (..)
    , startOptions
    , options
    ) where

import qualified Chess as C
import           System.Console.GetOpt( usageInfo
                                      , ArgOrder ( Permute ) 
                                      , OptDescr ( Option )
                                      , ArgDescr ( NoArg
                                                 , OptArg
                                                 , ReqArg
                                                 )
                                      )
import           System.Exit ( ExitCode (ExitFailure, ExitSuccess) 
                             , exitWith 
                             )
import           System.IO ( hPutStrLn
                           , Handle
                           , stderr
                           , stdin
                           )

data Options = Options { optInteractiveColor :: Maybe C.Color
                       }

startOptions :: Options
startOptions = Options { optInteractiveColor = Nothing
                       }

options :: [OptDescr ( Options -> IO (Options) ) ]
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
