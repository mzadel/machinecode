
--
-- SpecAstToCodeParser.hs
--

-- functions to convert a SpecAst node to a parser that goes from bits to
-- CodeAst nodes

-- TODO: decouple this from the DCPU-specific parts

module SpecAstToCodeParser where

import SpecAstToCodeParserInternal
import qualified SpecAst as S
import qualified CodeAst as C
import Data.Bit
import Text.Parsec.Prim
import Text.Parsec.Combinator (choice)
import StreamInstance

type Parser = Parsec [Bit] ()

-- takes a character string representing bits expected (one char per bit, from
-- '0','1', [A-Z] or [a-z] (no spaces allowed!)) and returns a parser
-- that will match those bits
bitSpecToParser :: String -> Parser [Bit]
bitSpecToParser bitchars = sequence (map bitchartoparser bitchars)
    where
        bitchartoparser '0' = matchBit 0
        bitchartoparser '1' = matchBit 1
        bitchartoparser _ = anyBit

-- signature for the function to convert a spec field to a code field
-- spec field, (field string -> parsed contents -> output field type) -> Parser for that field type
specFieldToCodeFieldParser :: S.Field -> (String -> String -> a) -> Parser (C.Field a)
specFieldToCodeFieldParser _ _ = return (C.Field ())

specFieldToCodeFieldParser (S.Field S.Literal specstring) convert =

{-
-- so here should there be that function that takes the spec string and the
-- literal bits, and spits out an interpretation?
-- literal :: S.Field -> (String -> [Bit] -> a) -> Parser (C.Field a)
literal :: S.Field -> a -> Parser (C.Field a)
literal (S.Field _ payload) fieldtype = do
    bitsparsed <- bitSpecToParser payload
    return (C.Field fieldtype bitsparsed)

-- this needs to be in the instruction set specification, and should be passed
-- in to the functions that need it
interpretSpecSubstring :: String -> C.DcpuFieldType
interpretSpecSubstring "Aaaaaa" = C.DcpuRegA
interpretSpecSubstring "Bbbbb" = C.DcpuRegB
interpretSpecSubstring "Dddddddddddddddd" = C.DcpuOptionalWord

variable :: S.Field -> Parser C.DcpuField
variable (S.Field S.Variable payload) = do
    bitsparsed <- bitSpecToParser payload
    return (C.DcpuField fieldtype bitsparsed description)
    where
        description = payload
        fieldtype = interpretSpecSubstring payload
-}
specFieldType :: S.Field -> S.FieldType
specFieldType (S.Field t _) = t

specFieldToParser :: S.Field -> Parser C.Field
specFieldToParser field = dispatchfieldtype (specFieldType field) $ field
    where
        dispatchfieldtype S.Literal = literal
        dispatchfieldtype S.Variable = variable

specFieldsToParsers :: [S.Field] -> [Parser C.Field]
specFieldsToParsers specs = map specFieldToParser specs

specToParser :: S.InstructionSpec -> Parser C.Instruction
specToParser (S.InstructionSpec name fields) = do
    parsedfields <- sequence $ specFieldsToParsers fields
    return (C.Instruction name parsedfields)

specsToParser :: [S.InstructionSpec] -> Parser C.Instruction
specsToParser specs = choice $ map (try . specToParser) specs


-- vim:sw=4:ts=4:et:ai:
