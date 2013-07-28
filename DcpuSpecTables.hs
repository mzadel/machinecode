
--
-- DcpuSpecTables.hs
--

module DcpuSpecTables where

import BitList (bitsFromByte)
import Data.Word (Word8)
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


-- this next thing should include: string to be mapped from, then field type,
-- then annotation
-- ( "Aaaaaa", "010101", DcpuRegA, "this means yadda" )
-- then maybe I can can curry these?  like partially apply "Aaaaaa"
-- and then apply the "010101", and then get the values back
--
-- Maybe I should make the field description part of the field type
--
-- Also, I need to include the ability to add functions for interpreting a
-- given value and pretty-printing the value

-- list reg a interpretations in hex.  do it this way so it matches the spec
-- document, even though we only use the 6 lower-order bits.  regaspecshex is
-- converted to regaspecs, which specifies the register a contents as a Char
-- bit spec string
regaspecshex = [

        ( 0x00, "A" ),
        ( 0x01, "B" ),
        ( 0x02, "C" ),
        ( 0x03, "X" ),
        ( 0x04, "Y" ),
        ( 0x05, "Z" ),
        ( 0x06, "I" ),
        ( 0x07, "J" ),
        ( 0x08, "[A]" ),
        ( 0x09, "[B]" ),
        ( 0x0a, "[C]" ),
        ( 0x0b, "[X]" ),
        ( 0x0c, "[Y]" ),
        ( 0x0d, "[Z]" ),
        ( 0x0e, "[I]" ),
        ( 0x0f, "[J]" ),
        ( 0x10, "[A + next word]" ),
        ( 0x11, "[B + next word]" ),
        ( 0x12, "[C + next word]" ),
        ( 0x13, "[X + next word]" ),
        ( 0x14, "[Y + next word]" ),
        ( 0x15, "[Z + next word]" ),
        ( 0x16, "[I + next word]" ),
        ( 0x17, "[J + next word]" ),
        ( 0x18, "(POP / [SP++])" ),
        ( 0x19, "[SP] / PEEK" ),
        ( 0x1a, "[SP + next word] / PICK n" ),
        ( 0x1b, "SP" ),
        ( 0x1c, "PC" ),
        ( 0x1d, "EX" ),
        ( 0x1e, "[next word]" ),
        ( 0x1f, "next word (literal)" ),

        ( 0x20, "literal -1" ),
        ( 0x21, "literal 0" ),
        ( 0x22, "literal 1" ),
        ( 0x23, "literal 2" ),
        ( 0x24, "literal 3" ),
        ( 0x25, "literal 4" ),
        ( 0x26, "literal 5" ),
        ( 0x27, "literal 6" ),
        ( 0x28, "literal 7" ),
        ( 0x29, "literal 8" ),
        ( 0x2a, "literal 9" ),
        ( 0x2b, "literal 10" ),
        ( 0x2c, "literal 11" ),
        ( 0x2d, "literal 12" ),
        ( 0x2e, "literal 13" ),
        ( 0x2f, "literal 14" ),
        ( 0x30, "literal 15" ),
        ( 0x31, "literal 16" ),
        ( 0x32, "literal 17" ),
        ( 0x33, "literal 18" ),
        ( 0x34, "literal 19" ),
        ( 0x35, "literal 20" ),
        ( 0x36, "literal 21" ),
        ( 0x37, "literal 22" ),
        ( 0x38, "literal 23" ),
        ( 0x39, "literal 24" ),
        ( 0x3a, "literal 25" ),
        ( 0x3b, "literal 26" ),
        ( 0x3c, "literal 27" ),
        ( 0x3d, "literal 28" ),
        ( 0x3e, "literal 29" ),
        ( 0x3f, "literal 30" )

    ]

-- compute a bit string from the input byte from its low order bits, listed
-- most significant bit first
lobitstring :: Int -> Word8 -> [Bit]
lobitstring length inbyte = drop (8-length) $ bitsFromByte inbyte

-- compute the approprate bit string from the hex value
regaspecs :: [ ([Bit],String) ]
regaspecs = zip (map (lobitstring 6) vals) labels
    where
        (vals,labels) = unzip regaspecshex



-- idea for mapping between the variable fields: use pattern matching in a
-- function like this
--mapvarfield "Aaaaaa" "010010" = ( DcpuRegA, "A" )
--mapvarfield "Aaaaaa" "010100" = ( DcpuRegA, "B" )


-- vim:sw=4:ts=4:et:ai:
