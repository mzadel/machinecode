
--
-- Main.hs
--

import DcpuSpecTables
import BitList (bitsFromByteList)
import CodeAst
import CodeParser
import Data.Either (rights)
import Text.Parsec.Prim
import Text.Parsec.Error (ParseError)

input = bitsFromByteList [
    0x00, 0x20,
    0x01, 0x00,
    0x01, 0x20,
    0x01, 0x40,
    0x01, 0x60,
    0x01, 0x80,
    0x02, 0x00,
    0x02, 0x20,
    0x02, 0x40
    ]

dcpuParser = codeparser instrspecs fieldtable

a :: Either ParseError [Instruction String (DcpuFieldType,String)]
a = parse dcpuParser "file N/A" input

parsed = head $ rights [a]

main = do
    print $ length parsed
    mapM_ (putStrLn . show) parsed


-- vim:sw=4:ts=4:et:ai:
