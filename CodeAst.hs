
--
-- CodeAst.hs
--

module CodeAst where

import Data.Bit

data FieldType = Literal | Variable
    deriving (Show)

data Field a = Field FieldType a [Bit]
    deriving (Show)

data Instruction a b = Instruction a [Field b]
    deriving (Show)

-- vim:sw=4:ts=4:et:ai:
