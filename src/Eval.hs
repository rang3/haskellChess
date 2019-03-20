module Eval
    ( eval
    ) where

import qualified Chess as C

eval :: C.Board -> Float
eval b | C.mate C.White b = -1
       | C.mate C.Black b = 1
       | otherwise      = score b

score :: C.Board -> Float
score = undefined
