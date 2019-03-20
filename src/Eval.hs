module Eval
    ( eval
    ) where

import qualified Chess as C
import           Data.Maybe ( catMaybes )

eval :: C.Board -> Float
eval b | C.mate C.White b = -1
       | C.mate C.Black b = 1
       | C.stalemate C.Black b = 0
       | C.stalemate C.White b = 0
       | otherwise      = sigmoid $ score b

sigmoid :: Float -> Float
sigmoid x = (exp x) / ((exp x) + 1)

score :: C.Board -> Float
score b = 1.0 * (materialScore b)

materialScore :: C.Board -> Float
materialScore b = whiteMaterial - blackMaterial
    where whiteMaterial = sum $ map (value) (materialForColor C.White b)
          blackMaterial = sum $ map (value) (materialForColor C.Black b)

materialForColor :: C.Color -> C.Board -> [C.Piece]
materialForColor c b = filter (\p -> C.clr p == c) (allPieces b)

allPieces :: C.Board -> [C.Piece]
allPieces b = catMaybes $ map (\(f,r) -> C.pieceAt f r b)
    [(f,r) | f <- [0..7], r <- [0..7]]

value :: C.Piece -> Float
value (C.Piece _ C.King) = 0
value (C.Piece _ C.Queen) = 9
value (C.Piece _ C.Rook) = 5
value (C.Piece _ C.Bishop) = 3
value (C.Piece _ C.Knight) = 3
value (C.Piece _ C.Pawn) = 1
