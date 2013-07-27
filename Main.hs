
--
-- Main.hs
--

import DcpuSpecTables (instrspecs)
import BitList (bitsFromByteList)
import SpecParser
import Text.Parsec.Prim
import SpecAstToCodeParser
import CodeAst
import Data.Either (rights)
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

specasts = rights [ specToAst spec label | (spec,label) <- instrspecs ]

theparser :: Parser [DcpuInstruction]
theparser = many $ specsToParser specasts

a :: Either ParseError [DcpuInstruction]
a = parse theparser "file N/A" input

parsed = head $ rights [a]

main = do
    print $ length parsed
    mapM_ (putStrLn . show) parsed


-- vim:sw=4:ts=4:et:ai:
