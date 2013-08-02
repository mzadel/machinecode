
--
-- SpecAst.hs
--

module SpecAst where

data Field = FieldLiteral String | FieldVariable String
    deriving (Show)
data InstructionSpec = InstructionSpec String [Field]
    deriving (Show)

-- Field: string is the parseable string "0100" or "Aaaa"
-- InstructionSpec: the string is the name/interpretation of the instruction
-- (ie opcode)

class WithString a where
    len :: a -> Int
    string :: a -> String

instance WithString Field where
    len (FieldLiteral str) = length str
    len (FieldVariable str) = length str
    string (FieldLiteral str) = str
    string (FieldVariable str) = str

-- vim:sw=4:ts=4:et:ai:
