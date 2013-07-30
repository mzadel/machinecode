
--
-- CodeAst.hs
--

module CodeAst where

import Data.Bit

data FieldType = Literal | Variable
    deriving (Show)

-- maybe instead have a FieldLiteral type constructor and a FieldVariable type
-- constructor, so the literal one doesn't have to know about a and so it won't
-- have to be specified in the covert function
data Field a = Field FieldType a [Bit]
    deriving (Show)

data Instruction a b = Instruction a [Field b]
    deriving (Show)

-- vim:sw=4:ts=4:et:ai:
