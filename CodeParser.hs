
--
-- CodeParser.hs
--

import FieldInterpreter -- move this code here afterward
import SpecAstToCodeParser
import SpecParser
import qualified SpecAst as S
import qualified CodeAst as C
import Data.Either (rights)
import Text.Parsec.Prim
import Data.Bit

codeparser :: [(String,String)] -> (String->[Bit]->a) -> Parser [C.Instruction String a]
codeparser instrspecs convert = many $ instructionparser
    where
        specasts = rights [ specToAst spec label | (spec,label) <- instrspecs ]
        instructionparser = specsToParser convert specasts




-- vim:sw=4:ts=4:et:ai:
