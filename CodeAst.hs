
--
-- CodeAst.hs
--

module CodeAst where

import Data.Bit

-- this should be an instance of a typeclass that I can use to write a
-- polymorphic function on instructions
-- this is maybe the only thing the end user needs to define
data DcpuFieldType = DcpuLiteralBits | DcpuRegA | DcpuRegB | DcpuOptionalWord
    deriving (Show)

-- this can be generic, not specific to dcpu
-- field type, its payload (string of bits), interpretation
data DcpuField = DcpuField DcpuFieldType [Bit] String
    deriving (Show)

-- this can be generic, not specific to dcpu
data DcpuInstruction = DcpuInstruction String [DcpuField]
    deriving (Show)

-- vim:sw=4:ts=4:et:ai:
