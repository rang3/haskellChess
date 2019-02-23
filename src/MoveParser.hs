module MoveParser
    ( parseMove
    ) where

import           Board (Coordinate(..), Move(..))

import qualified Text.Read as R

parseMove :: String -> Maybe Move
parseMove [f1,r1,'-',f2,r2] = let mc1 = parseCoordinate [f1,r1]
                                  mc2 = parseCoordinate [f2,r2]
                              in  case (mc1,mc2) of
                                      (Just  c1, Just c2) -> Just $ Move c1 c2
                                      (_,_)               ->  Nothing
parseMove _ = Nothing
    
parseCoordinate :: String -> Maybe Coordinate
parseCoordinate [f,r] = let (fi, ri) = (parseFile f, parseRank r) 
    in if validFileOrRank fi && validFileOrRank ri
          then Just $ Coordinate fi ri
          else Nothing
parseCoordinate _ = Nothing

validFileOrRank :: Int -> Bool
validFileOrRank i 
    | i <= 7 && i >= 0 = True
    | otherwise = False

parseFile :: Char -> Int
parseFile c = letterToNumber c

letterToNumber :: Char -> Int
letterToNumber 'a' = 0
letterToNumber 'b' = 1
letterToNumber 'c' = 2
letterToNumber 'd' = 3
letterToNumber 'e' = 4
letterToNumber 'f' = 5
letterToNumber 'g' = 6
letterToNumber 'h' = 7
letterToNumber _   = -1

parseRank :: Char -> Int
parseRank c = let r = (R.readMaybe [c] :: Maybe Int)
    in case r of
        Just ri -> ri - 1
        otherwise -> -1
