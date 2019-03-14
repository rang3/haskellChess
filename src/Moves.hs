module Moves
    ( Coordinate ( Coordinate )
    , Move ( Move ) 
    , nextGameStates
    ) where

import           Data.Char (ord, chr)
import qualified Text.Read as R

data Move = Move Coordinate Coordinate
    deriving (Eq)

instance Show Move where
  show (Move x y) = (show x) ++ "-" ++ (show y)

data Coordinate = Coordinate Int Int
    deriving (Eq)

instance Show Coordinate where
  show (Coordinate x y) = let row = chr(ord 'a'+x)
                              rank = show (y+1)
                          in row:rank

nextGameStates = undefined
