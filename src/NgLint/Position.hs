module NgLint.Position where

import Text.Parsec.Pos

class Position a where
    getPos :: a -> SourcePos
