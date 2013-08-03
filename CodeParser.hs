
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

matchtableentries :: [( String, [Bit], a, u->u )] -> String -> [Bit] -> [( String, [Bit], a, u->u )]
matchtableentries table specstring parsedbits = filter ismatch table
    where
        ismatch (s,b,_,_) = s == specstring && (b == parsedbits || null parsedbits)

-- take a spec string and the bits found in them, and return a label
-- based on the lookup table
fieldlabeler :: [( String, [Bit], a, u->u )] -> String -> [Bit] -> a
fieldlabeler table specstring parsedbits = head matchinglabels
    where
        matchinglabels = map extractlabel matchingtuples
        extractlabel (_,_,l,_) = l
        matchingtuples = matchtableentries table specstring parsedbits

getstatetransform :: [( String, [Bit], a, u->u )] -> String -> [Bit] -> (u -> u)
getstatetransform table specstring parsedbits = head matchingstatetransforms
    where
        matchingstatetransforms = map extractstatetransform matchingtuples
        extractstatetransform (_,_,_,f) = f
        matchingtuples = matchtableentries table specstring parsedbits

codeparser :: [(String,a)] -> [( String, [Bit], b, u->u )] -> (String -> u -> Bool) -> Parser u [C.Instruction a b]
codeparser instrspecs fieldlabeltable shouldparse = many $ instructionparser
    where
        specasts = rights [ specToAst spec label | (spec,label) <- instrspecs ]
        instructionparser = specsToParser labeler statetransformer shouldparse specasts
        labeler = fieldlabeler fieldlabeltable
        statetransformer = getstatetransform fieldlabeltable

-- vim:sw=4:ts=4:et:ai:
