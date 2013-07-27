
--
-- CodeAst.hs
--

module CodeAst where

import Data.Bit

data Field a = Field a [Bit]
    deriving (Show)

data Instruction a = Instruction a [Field a]
    deriving (Show)

-- vim:sw=4:ts=4:et:ai:
