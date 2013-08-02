
--
-- CodeParser.hs
--

import SpecAstToCodeParser
import SpecParser
import qualified SpecAst as S
import qualified CodeAst as C
import Data.Either (rights)
import Text.Parsec.Prim
import Data.Bit

-- take a spec string and the bits found in them, and return an interpretation
-- based on the lookup table
fieldinterpreter :: [( String, [Bit], a )] -> String -> [Bit] -> a
fieldinterpreter table specstring parsedbits = head matchinglabels
    where
        (strings,bits,labels) = unzip3 table
        matchinglabels = [ l | s <- strings, b <- bits, l <- labels, s == specstring, b == parsedbits ]

codeparser :: [(String,String)] -> (String->[Bit]->a) -> Parser [C.Instruction String a]
codeparser instrspecs convert = many $ instructionparser
    where
        specasts = rights [ specToAst spec label | (spec,label) <- instrspecs ]
        instructionparser = specsToParser convert specasts




-- vim:sw=4:ts=4:et:ai:
