
--
-- DcpuSpecTables.hs
--

module DcpuSpecTables (DcpuFieldType) where

import BitList (bitsFromByte)
import Data.Bit  (Bit)

data DcpuFieldType = DcpuLiteralBits | DcpuRegA | DcpuRegB | DcpuOptionalWord
    deriving (Show)

instrspecs = [
        ( "Aaaaaa0000100000", "JSR a" ),
        ( "Aaaaaa0100000000", "INT a" ),
        ( "Aaaaaa0100100000", "IAG a" ),
        ( "Aaaaaa0101000000", "IAS a" ),
        ( "Aaaaaa0101100000", "RFI a" ),
        ( "Aaaaaa0110000000", "IAQ a" ),
        ( "Aaaaaa1000000000", "HWN a" ),
        ( "Aaaaaa1000100000", "HWQ a" ),
        ( "Aaaaaa1001000000", "HWI a" )
    ]


fieldtable :: [( String, [Bit], (DcpuFieldType, String) )]
fieldtable = [

        ( "Aaaaaa", drop 2 $ bitsFromByte  0x00, ( DcpuRegA, "A" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x01, ( DcpuRegA, "B" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x02, ( DcpuRegA, "C" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x03, ( DcpuRegA, "X" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x04, ( DcpuRegA, "Y" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x05, ( DcpuRegA, "Z" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x06, ( DcpuRegA, "I" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x07, ( DcpuRegA, "J" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x08, ( DcpuRegA, "[A]" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x09, ( DcpuRegA, "[B]" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x0a, ( DcpuRegA, "[C]" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x0b, ( DcpuRegA, "[X]" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x0c, ( DcpuRegA, "[Y]" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x0d, ( DcpuRegA, "[Z]" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x0e, ( DcpuRegA, "[I]" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x0f, ( DcpuRegA, "[J]" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x10, ( DcpuRegA, "[A + next word]" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x11, ( DcpuRegA, "[B + next word]" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x12, ( DcpuRegA, "[C + next word]" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x13, ( DcpuRegA, "[X + next word]" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x14, ( DcpuRegA, "[Y + next word]" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x15, ( DcpuRegA, "[Z + next word]" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x16, ( DcpuRegA, "[I + next word]" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x17, ( DcpuRegA, "[J + next word]" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x18, ( DcpuRegA, "(POP / [SP++])" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x19, ( DcpuRegA, "[SP] / PEEK" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x1a, ( DcpuRegA, "[SP + next word] / PICK n" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x1b, ( DcpuRegA, "SP" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x1c, ( DcpuRegA, "PC" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x1d, ( DcpuRegA, "EX" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x1e, ( DcpuRegA, "[next word]" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x1f, ( DcpuRegA, "next word (literal)" ) ),

        ( "Aaaaaa", drop 2 $ bitsFromByte  0x20, ( DcpuRegA, "literal -1" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x21, ( DcpuRegA, "literal 0" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x22, ( DcpuRegA, "literal 1" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x23, ( DcpuRegA, "literal 2" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x24, ( DcpuRegA, "literal 3" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x25, ( DcpuRegA, "literal 4" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x26, ( DcpuRegA, "literal 5" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x27, ( DcpuRegA, "literal 6" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x28, ( DcpuRegA, "literal 7" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x29, ( DcpuRegA, "literal 8" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x2a, ( DcpuRegA, "literal 9" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x2b, ( DcpuRegA, "literal 10" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x2c, ( DcpuRegA, "literal 11" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x2d, ( DcpuRegA, "literal 12" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x2e, ( DcpuRegA, "literal 13" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x2f, ( DcpuRegA, "literal 14" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x30, ( DcpuRegA, "literal 15" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x31, ( DcpuRegA, "literal 16" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x32, ( DcpuRegA, "literal 17" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x33, ( DcpuRegA, "literal 18" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x34, ( DcpuRegA, "literal 19" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x35, ( DcpuRegA, "literal 20" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x36, ( DcpuRegA, "literal 21" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x37, ( DcpuRegA, "literal 22" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x38, ( DcpuRegA, "literal 23" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x39, ( DcpuRegA, "literal 24" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x3a, ( DcpuRegA, "literal 25" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x3b, ( DcpuRegA, "literal 26" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x3c, ( DcpuRegA, "literal 27" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x3d, ( DcpuRegA, "literal 28" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x3e, ( DcpuRegA, "literal 29" ) ),
        ( "Aaaaaa", drop 2 $ bitsFromByte  0x3f, ( DcpuRegA, "literal 30" ) )

        ]


{-
-- this is a little complicated for this file
dcpuParser :: Parser [Code.Instruction String (DcpuFieldType, String)]
dcpuParser = many $ instructionparser
    where
        specasts = rights [ specToAst spec label | (spec,label) <- instrspecs ]
        instructionparser = specsToParser convert specasts
-}

-- vim:sw=4:ts=4:et:ai:
