
--
-- SpecAstToCodeParser.hs
--

-- functions to convert a SpecAst node to a parser that goes from bits to
-- CodeAst nodes

module SpecAstToCodeParser where

import SpecAstToCodeParserInternal
import qualified SpecAst as S
import qualified CodeAst as C
import Data.Bit
import Text.Parsec.Prim
import Text.Parsec.Combinator (choice)
import Text.Parsec.String () -- for Stream instance

type Parser u = Parsec [Bit] u

-- takes a character string representing bits expected (one char per bit, from
-- '0','1', [A-Z] or [a-z] (no spaces allowed!)) and returns a parser
-- that will match those bits
bitSpecToParser :: String -> Parser u [Bit]
bitSpecToParser bitchars = sequence (map bitchartoparser bitchars)
    where
        bitchartoparser '0' = matchBit 0
        bitchartoparser '1' = matchBit 1
        bitchartoparser _ = anyBit

-- signature for the function to convert a spec field to a code field
-- spec field, (field string -> parsed contents -> output field type) -> Parser for that field type
specFieldToCodeFieldParser :: (String -> [Bit] -> a) -> (String -> [Bit] -> (u -> u)) -> (String -> u -> Bool) -> S.Field -> Parser u (C.Field a)

specFieldToCodeFieldParser _ _ _ (S.FieldLiteral specstring) = do
    bitsparsed <- bitSpecToParser specstring
    return (C.FieldLiteral bitsparsed)

specFieldToCodeFieldParser convert statetransformer shouldparse (S.FieldVariable specstring) = do
    st <- getState
    if ( shouldparse specstring st )
    then
        do
            bitsparsed <- bitSpecToParser specstring
            modifyState (statetransformer specstring bitsparsed)
            return (C.FieldVariable (convert specstring bitsparsed) bitsparsed)
    else
        return C.FieldNothing

-- convert one spec to a parser for that spec
specToParser :: (String -> [Bit] -> b) -> (String -> [Bit] -> (u -> u)) -> (String -> u -> Bool) -> u -> S.InstructionSpec a -> Parser u (C.Instruction a b)
specToParser convert statetransformer shouldparse defaultstate (S.InstructionSpec name fields) = do
    putState defaultstate
    parsedfields <- sequence $ map (specFieldToCodeFieldParser convert statetransformer shouldparse) fields
    return (C.Instruction name parsedfields)

specsToParser :: (String -> [Bit] -> b) -> (String -> [Bit] -> (u -> u)) -> (String -> u -> Bool) -> u -> [S.InstructionSpec a] -> Parser u (C.Instruction a b)
specsToParser convert statetransformer shouldparse defaultstate specs = choice $ map (try . specToParser convert statetransformer shouldparse defaultstate) specs

-- vim:sw=4:ts=4:et:ai:
