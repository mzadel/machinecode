
--
-- DcpuSpecTables.hs
--

module DcpuSpecTables where

import BitList (bitsFromByte)
import Data.Bit  (Bit)

data FieldType = RegA | RegB | OptionalWord
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

-- Compute a bit string from an int value.  I'm expressing these in hex values
-- so they match the dcpu instruction specification, so they'll be easier to
-- compare.
toRegABits = drop 2 . bitsFromByte

fieldlabeltable :: [( String, [Bit], (FieldType, String) )]
fieldlabeltable = [

        ( "Aaaaaa", toRegABits 0x00, ( RegA, "A" ) ),
        ( "Aaaaaa", toRegABits 0x01, ( RegA, "B" ) ),
        ( "Aaaaaa", toRegABits 0x02, ( RegA, "C" ) ),
        ( "Aaaaaa", toRegABits 0x03, ( RegA, "X" ) ),
        ( "Aaaaaa", toRegABits 0x04, ( RegA, "Y" ) ),
        ( "Aaaaaa", toRegABits 0x05, ( RegA, "Z" ) ),
        ( "Aaaaaa", toRegABits 0x06, ( RegA, "I" ) ),
        ( "Aaaaaa", toRegABits 0x07, ( RegA, "J" ) ),
        ( "Aaaaaa", toRegABits 0x08, ( RegA, "[A]" ) ),
        ( "Aaaaaa", toRegABits 0x09, ( RegA, "[B]" ) ),
        ( "Aaaaaa", toRegABits 0x0a, ( RegA, "[C]" ) ),
        ( "Aaaaaa", toRegABits 0x0b, ( RegA, "[X]" ) ),
        ( "Aaaaaa", toRegABits 0x0c, ( RegA, "[Y]" ) ),
        ( "Aaaaaa", toRegABits 0x0d, ( RegA, "[Z]" ) ),
        ( "Aaaaaa", toRegABits 0x0e, ( RegA, "[I]" ) ),
        ( "Aaaaaa", toRegABits 0x0f, ( RegA, "[J]" ) ),
        ( "Aaaaaa", toRegABits 0x10, ( RegA, "[A + next word]" ) ),
        ( "Aaaaaa", toRegABits 0x11, ( RegA, "[B + next word]" ) ),
        ( "Aaaaaa", toRegABits 0x12, ( RegA, "[C + next word]" ) ),
        ( "Aaaaaa", toRegABits 0x13, ( RegA, "[X + next word]" ) ),
        ( "Aaaaaa", toRegABits 0x14, ( RegA, "[Y + next word]" ) ),
        ( "Aaaaaa", toRegABits 0x15, ( RegA, "[Z + next word]" ) ),
        ( "Aaaaaa", toRegABits 0x16, ( RegA, "[I + next word]" ) ),
        ( "Aaaaaa", toRegABits 0x17, ( RegA, "[J + next word]" ) ),
        ( "Aaaaaa", toRegABits 0x18, ( RegA, "(POP / [SP++])" ) ),
        ( "Aaaaaa", toRegABits 0x19, ( RegA, "[SP] / PEEK" ) ),
        ( "Aaaaaa", toRegABits 0x1a, ( RegA, "[SP + next word] / PICK n" ) ),
        ( "Aaaaaa", toRegABits 0x1b, ( RegA, "SP" ) ),
        ( "Aaaaaa", toRegABits 0x1c, ( RegA, "PC" ) ),
        ( "Aaaaaa", toRegABits 0x1d, ( RegA, "EX" ) ),
        ( "Aaaaaa", toRegABits 0x1e, ( RegA, "[next word]" ) ),
        ( "Aaaaaa", toRegABits 0x1f, ( RegA, "next word (literal)" ) ),

        ( "Aaaaaa", toRegABits 0x20, ( RegA, "literal -1" ) ),
        ( "Aaaaaa", toRegABits 0x21, ( RegA, "literal 0" ) ),
        ( "Aaaaaa", toRegABits 0x22, ( RegA, "literal 1" ) ),
        ( "Aaaaaa", toRegABits 0x23, ( RegA, "literal 2" ) ),
        ( "Aaaaaa", toRegABits 0x24, ( RegA, "literal 3" ) ),
        ( "Aaaaaa", toRegABits 0x25, ( RegA, "literal 4" ) ),
        ( "Aaaaaa", toRegABits 0x26, ( RegA, "literal 5" ) ),
        ( "Aaaaaa", toRegABits 0x27, ( RegA, "literal 6" ) ),
        ( "Aaaaaa", toRegABits 0x28, ( RegA, "literal 7" ) ),
        ( "Aaaaaa", toRegABits 0x29, ( RegA, "literal 8" ) ),
        ( "Aaaaaa", toRegABits 0x2a, ( RegA, "literal 9" ) ),
        ( "Aaaaaa", toRegABits 0x2b, ( RegA, "literal 10" ) ),
        ( "Aaaaaa", toRegABits 0x2c, ( RegA, "literal 11" ) ),
        ( "Aaaaaa", toRegABits 0x2d, ( RegA, "literal 12" ) ),
        ( "Aaaaaa", toRegABits 0x2e, ( RegA, "literal 13" ) ),
        ( "Aaaaaa", toRegABits 0x2f, ( RegA, "literal 14" ) ),
        ( "Aaaaaa", toRegABits 0x30, ( RegA, "literal 15" ) ),
        ( "Aaaaaa", toRegABits 0x31, ( RegA, "literal 16" ) ),
        ( "Aaaaaa", toRegABits 0x32, ( RegA, "literal 17" ) ),
        ( "Aaaaaa", toRegABits 0x33, ( RegA, "literal 18" ) ),
        ( "Aaaaaa", toRegABits 0x34, ( RegA, "literal 19" ) ),
        ( "Aaaaaa", toRegABits 0x35, ( RegA, "literal 20" ) ),
        ( "Aaaaaa", toRegABits 0x36, ( RegA, "literal 21" ) ),
        ( "Aaaaaa", toRegABits 0x37, ( RegA, "literal 22" ) ),
        ( "Aaaaaa", toRegABits 0x38, ( RegA, "literal 23" ) ),
        ( "Aaaaaa", toRegABits 0x39, ( RegA, "literal 24" ) ),
        ( "Aaaaaa", toRegABits 0x3a, ( RegA, "literal 25" ) ),
        ( "Aaaaaa", toRegABits 0x3b, ( RegA, "literal 26" ) ),
        ( "Aaaaaa", toRegABits 0x3c, ( RegA, "literal 27" ) ),
        ( "Aaaaaa", toRegABits 0x3d, ( RegA, "literal 28" ) ),
        ( "Aaaaaa", toRegABits 0x3e, ( RegA, "literal 29" ) ),
        ( "Aaaaaa", toRegABits 0x3f, ( RegA, "literal 30" ) )

    ]

-- vim:sw=4:ts=4:et:ai:
