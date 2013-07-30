
--
-- CodeAst.hs
--

module CodeAst where

import Data.Bit

data Field a = FieldLiteral [Bit] | FieldVariable a [Bit] 
    deriving (Show)

data Instruction a b = Instruction a [Field b]
    deriving (Show)

-- vim:sw=4:ts=4:et:ai:
