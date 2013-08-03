
--
-- Main.hs
--

import qualified DcpuSpecTables as Dcpu
import BitList (bitsFromByteList)
import CodeAst
import CodeParser
import Data.Either (rights)
import Text.Parsec.Prim
import Text.Parsec.Error (ParseError)

input = bitsFromByteList [ 0x7c, 0x01, 0x00, 0x30 ]

dcpuParser = codeparser Dcpu.instrspecs Dcpu.fieldlabeltable Dcpu.shouldparsefield Dcpu.defaultstate

a :: Either ParseError [Instruction String (Dcpu.FieldType,String)]
a = runParser dcpuParser Dcpu.defaultstate "file N/A" input

parsed :: [Instruction String (Dcpu.FieldType,String)]
parsed = head $ rights [a]

main = do
    print $ length parsed
    mapM_ (putStrLn . show) parsed


-- vim:sw=4:ts=4:et:ai:
