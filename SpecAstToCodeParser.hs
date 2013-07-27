
--
-- SpecAstToCodeParser.hs
--

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- functions to convert a SpecAst node to a parser that goes from bits to
-- CodeAst nodes

-- TODO: decouple this from the DCPU-specific parts

module SpecAstToCodeParser where

import SpecAstToCodeParserInternal
import qualified SpecAst as Spec
import qualified CodeAst as Code
import Data.Bit
import Text.Parsec.Prim
import Text.Parsec.Combinator (choice)
import StreamInstance

type Parser = Parsec [Bit] ()

-- takes a character string representing bits expected (one char per bit, from
-- '0','1', [A-Z] or [a-z] (no spaces allowed!)) and returns a list of parsers
-- that will match those bits
bitSpecToParsers :: String -> [Parser Bit]
bitSpecToParsers bitchars = map bitchartoparser bitchars
    where
        bitchartoparser '0' = matchBit 0
        bitchartoparser '1' = matchBit 1
        bitchartoparser _ = anyBit

literal :: Code.FieldLabel a => Spec.Field -> a -> Parser (Code.Field a)
literal (Spec.Field label payload) fieldtype = do
    bitsparsed <- sequence (bitSpecToParsers payload)
    return (Code.Field fieldtype bitsparsed)
{-
-- this needs to be in the instruction set specification, and should be passed
-- in to the functions that need it
interpretSpecSubstring :: String -> Code.DcpuFieldType
interpretSpecSubstring "Aaaaaa" = Code.DcpuRegA
interpretSpecSubstring "Bbbbb" = Code.DcpuRegB
interpretSpecSubstring "Dddddddddddddddd" = Code.DcpuOptionalWord

variable :: Spec.Field -> Parser Code.DcpuField
variable (Spec.Field Spec.FieldVariable payload) = do
    bitsparsed <- sequence (bitSpecToParsers payload)
    return (Code.DcpuField fieldtype bitsparsed description)
    where
        description = payload
        fieldtype = interpretSpecSubstring payload

specFieldType :: Spec.Field -> Spec.FieldType
specFieldType (Spec.Field t _) = t

specFieldToParser :: Spec.Field -> Parser Code.DcpuField
specFieldToParser field = dispatchfieldtype (specFieldType field) $ field
    where
        dispatchfieldtype Spec.FieldLiteral = literal
        dispatchfieldtype Spec.FieldVariable = variable

specFieldsToParsers :: [Spec.Field] -> [Parser Code.DcpuField]
specFieldsToParsers specs = map specFieldToParser specs

specToParser :: Spec.InstructionSpec -> Parser Code.DcpuInstruction
specToParser (Spec.InstructionSpec name fields) = do
    parsedfields <- sequence $ specFieldsToParsers fields
    return (Code.DcpuInstruction name parsedfields)

specsToParser :: [Spec.InstructionSpec] -> Parser Code.DcpuInstruction
specsToParser specs = choice $ map (try . specToParser) specs
-}

-- vim:sw=4:ts=4:et:ai:
