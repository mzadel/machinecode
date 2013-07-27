
--
-- CodeAst.hs
--

module CodeAst where

import Data.Bit

data DcpuFieldType = DcpuLiteralBits | DcpuRegA | DcpuRegB | DcpuOptionalWord
    deriving (Show)

-- field type, its payload (string of bits), interpretation
data DcpuField = DcpuField DcpuFieldType [Bit] String
    deriving (Show)

data DcpuInstruction = DcpuInstruction String [DcpuField]
    deriving (Show)

-- vim:sw=4:ts=4:et:ai:
