
--
-- Main.hs
--

import qualified Pdp8SpecTables as Pdp8
import qualified Pretty
--import DcpuPretty
import BitList (bitsFromOctalWordList)
import CodeAst
import CodeParser
import Data.Either (rights)
import Text.Parsec.Prim
import Text.Parsec.Error (ParseError)
import Data.Bit (Bit)
import SpecAstToCodeParser (Parser)

input :: [Bit]
input = bitsFromOctalWordList [
    0o6032, 0o6031, 0o5357, 0o6036, 0o7106, 0o7006, 0o7510, 0o5357, 0o7006,
    0o6031, 0o5367, 0o6034, 0o7420, 0o3776, 0o3376, 0o5356, 0o0000
    ]

pdp8Parser :: Parser Pdp8.UserState [Instruction String (Pdp8.FieldType, String)]
pdp8Parser = codeparser Pdp8.instrspecs Pdp8.fieldlabeltable Pdp8.shouldparsefield Pdp8.defaultstate

a :: Either ParseError [Instruction String (Pdp8.FieldType,String)]
a = runParser pdp8Parser Pdp8.defaultstate "file N/A" input

parsed :: [Instruction String (Pdp8.FieldType,String)]
parsed = head $ rights [a]

main :: IO ()
main = do
    mapM_ (putStr . Pretty.printInstruction show) parsed


-- vim:sw=4:ts=4:et:ai:
