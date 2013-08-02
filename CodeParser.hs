
--
-- CodeParser.hs
--

import FieldInterpreter
import SpecAstToCodeParser
import qualified SpecAst as S
import qualified CodeAst as C

codeparser :: Parser [Code.Instruction String a]
codeparser = many $ instructionparser
    where
        specasts = rights [ specToAst spec label | (spec,label) <- instrspecs ]
        instructionparser = specsToParser convert specasts




-- vim:sw=4:ts=4:et:ai:
