
--
-- CodeParser.hs
--

module CodeParser (codeparser) where

import SpecAstToCodeParser
import SpecParser
import qualified SpecAst as S
import qualified CodeAst as C
import Data.Either (rights)
import Text.Parsec.Prim
import Data.Bit

matchtableentries :: [( String, [Bit], a, b->b )] -> String -> [Bit] -> [( String, [Bit], a, b->b )]
matchtableentries table specstring parsedbits = filter ismatch table
    where
        ismatch (s,b,l,x) = s == specstring && b == parsedbits

-- take a spec string and the bits found in them, and return an interpretation
-- based on the lookup table
fieldlabeler :: [( String, [Bit], a, b->b )] -> String -> [Bit] -> a
fieldlabeler table specstring parsedbits = head matchinglabels
    where
        matchinglabels = map extractlabel matchingtuples
        extractlabel (s,b,l,x) = l
        matchingtuples = matchtableentries table specstring parsedbits

codeparser :: [(String,String)] -> [( String, [Bit], a, b->b )] -> Parser [C.Instruction String a]
codeparser instrspecs fieldlabeltable = many $ instructionparser
    where
        specasts = rights [ specToAst spec label | (spec,label) <- instrspecs ]
        instructionparser = specsToParser labeler specasts
        labeler = fieldlabeler fieldlabeltable

-- vim:sw=4:ts=4:et:ai:
