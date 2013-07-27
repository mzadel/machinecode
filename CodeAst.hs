
--
-- CodeAst.hs
--

module CodeAst where

import Data.Bit

class FieldLabel a
class InstructionLabel a

data Field a = Field a [Bit]
    deriving (Show)

data Instruction a b = Instruction a [Field b]
    deriving (Show)

-- vim:sw=4:ts=4:et:ai:
