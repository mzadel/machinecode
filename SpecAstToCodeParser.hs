
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
specFieldToCodeFieldParser :: (String -> String -> a) -> S.Field -> Parser (C.Field a)

specFieldToCodeFieldParser _ (S.Field S.Literal specstring) = do
    bitsparsed <- bitSpecToParser specstring
    return (C.Field C.Literal () bitsparsed)

specFieldToCodeFieldParser convert (S.Field S.Literal specstring) = do
    bitsparsed <- bitSpecToParser specstring
    fieldtype <- convert specstring bitsparsed
    return (C.Field C.Variable fieldtype bitsparsed)

specFieldsToParsers :: (String -> String -> a) -> [S.Field] -> [Parser (C.Field a)]
specFieldsToParsers convert specs = map (specFieldToCodeFieldParser convert) specs

specToParser :: (String -> String -> a) -> S.InstructionSpec -> Parser (C.Instruction a b)
specToParser convert (S.InstructionSpec name fields) = do
    parsedfields <- sequence $ specFieldsToParsers convert fields
    return (C.Instruction name parsedfields)

specsToParser :: (String -> String -> a) -> [S.InstructionSpec] -> Parser (C.Instruction a b)
specsToParser convert specs = choice $ map (try . (specToParser convert)) specs

-- vim:sw=4:ts=4:et:ai:
