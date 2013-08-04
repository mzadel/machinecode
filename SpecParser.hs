
--
-- SpecParser.hs
--

module SpecParser (specToAst) where

import SpecAst
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import StreamInstance ()

type Parser = Parsec String ()

-- parse a subpart of the whole spec, and return an ast node that represents this
fieldliteral :: Parser Field
fieldliteral = do
    result <- many1 ( char '1' <|> char '0' )
    return $ FieldLiteral result

-- is there a simpler way to express this?
fieldvariable :: Parser Field
fieldvariable = do
    a <- upper
    rest <- many lower
    return $ FieldVariable ([a]++rest)

field :: Parser Field
field = do
    parsed <- fieldliteral <|> fieldvariable
    spaces  -- any optional spaces trailing each token
    return parsed

-- generate a parser that will convert from a spec string to an InstructionSpec
-- AST.  Also needs the interpretation of the instruction (ie the opcode).
parserfromspecstringtoast :: a -> Parser (InstructionSpec a)
parserfromspecstringtoast description = do
    spaces -- leading whitespace in the spec string
    fields <- many field
    eof
    return (InstructionSpec description fields)

specToAst :: String -> a -> Either ParseError (InstructionSpec a)
specToAst thespec thelabel = parse parser "path N/A" thespec
    where
        parser = parserfromspecstringtoast thelabel




-- vim:sw=4:ts=4:et:ai:
