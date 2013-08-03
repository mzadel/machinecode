
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
import StreamInstance ()

-- make state type a variable here so that I can defer binding it to the spec table
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
specFieldToCodeFieldParser :: (String -> [Bit] -> a) -> S.Field -> Parser (C.Field a)

specFieldToCodeFieldParser convert (S.FieldLiteral specstring) = do
    bitsparsed <- bitSpecToParser specstring
    return (C.FieldLiteral bitsparsed)

specFieldToCodeFieldParser convert (S.FieldVariable specstring) = do
    bitsparsed <- bitSpecToParser specstring
    return (C.FieldVariable (convert specstring bitsparsed) bitsparsed)


-- convert one spec to a parser for that spec
-- NB: we're hardcoding the first field of CodeAst.InstructionSpec (the
-- description) to be a string here.  Not sure how I'd do it otherwise yet.
specToParser :: (String -> [Bit] -> b) -> S.InstructionSpec a -> Parser (C.Instruction a b)
specToParser convert (S.InstructionSpec name fields) = do
    parsedfields <- sequence $ map (specFieldToCodeFieldParser convert) fields
    return (C.Instruction name parsedfields)

specsToParser :: (String -> [Bit] -> b) -> (c->c) -> [S.InstructionSpec a] -> Parser (C.Instruction a b)
specsToParser convert statetransformer specs = choice $ map (try . specToParser convert) specs

-- vim:sw=4:ts=4:et:ai:
