
--
-- Main.hs
--

import qualified DcpuSpecTables as Dcpu
import qualified Pretty
import DcpuPretty
import BitList (bitsFromByteList)
import CodeAst
import CodeParser
import Data.Either (rights)
import Text.Parsec.Prim
import Text.Parsec.Error (ParseError)
import Data.Bit (Bit)
import SpecAstToCodeParser (Parser)

input :: [Bit]
input = bitsFromByteList [
    0x7c, 0x01, 0x00, 0x30, 0x7f, 0xc1, 0x00, 0x20, 0x10, 0x00, 0x78, 0x03,
    0x10, 0x00, 0xc4, 0x13, 0x7f, 0x81, 0x00, 0x19, 0xac, 0xc1, 0x7c, 0x01,
    0x20, 0x00, 0x22, 0xc1, 0x20, 0x00, 0x88, 0xc3, 0x84, 0xd3, 0xbb, 0x81,
    0x94, 0x61, 0x7c, 0x20, 0x00, 0x17, 0x7f, 0x81, 0x00, 0x19, 0x94, 0x6f,
    0x63, 0x81, 0xeb, 0x81
    ]

dcpuParser :: Parser Dcpu.UserState [Instruction String (Dcpu.FieldType, String)]
dcpuParser = codeparser Dcpu.instrspecs Dcpu.fieldlabeltable Dcpu.shouldparsefield Dcpu.defaultstate

a :: Either ParseError [Instruction String (Dcpu.FieldType,String)]
a = runParser dcpuParser Dcpu.defaultstate "file N/A" input

parsed :: [Instruction String (Dcpu.FieldType,String)]
parsed = head $ rights [a]

main :: IO ()
main = do
    mapM_ (putStr . Pretty.printInstruction labeltostring) parsed


-- vim:sw=4:ts=4:et:ai:
