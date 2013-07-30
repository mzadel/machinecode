
--
-- DcpuSpecTables.hs
--

module DcpuSpecTables (DcpuFieldType, dcpuParser) where

-- this is packing quite a bit in here, and I want to keep it minimal so it's
-- simple for instruction set writers
import BitList (bitsFromByte)
import Data.Word (Word8)
import Data.Bit  (Bit)
import qualified CodeAst as Code
import qualified SpecAst as Spec
import SpecParser
import SpecAstToCodeParser
import Data.Either (rights)
import Text.Parsec.Prim
import Text.Parsec.Error (ParseError)

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


-- compute a bit string from the input byte from its low order bits, listed
-- most significant bit first
lobitstring :: Int -> Word8 -> [Bit]
lobitstring length inbyte = drop (8-length) $ bitsFromByte inbyte

convert :: String -> [Bit] -> (DcpuFieldType,String)
convert specstring parsedbits = case specstring of
    "literal" -> (DcpuLiteralBits,"literal")
    "Aaaaaa" -> convertAaaaaa parsedbits
    where
        convertAaaaaa bits
            | bits == lobitstring 6 0x00 = ( DcpuRegA, "A" )
            | bits == lobitstring 6 0x01 = ( DcpuRegA, "B" )
            | bits == lobitstring 6 0x02 = ( DcpuRegA, "C" )
            | bits == lobitstring 6 0x03 = ( DcpuRegA, "X" )
            | bits == lobitstring 6 0x04 = ( DcpuRegA, "Y" )
            | bits == lobitstring 6 0x05 = ( DcpuRegA, "Z" )
            | bits == lobitstring 6 0x06 = ( DcpuRegA, "I" )
            | bits == lobitstring 6 0x07 = ( DcpuRegA, "J" )
            | bits == lobitstring 6 0x08 = ( DcpuRegA, "[A]" )
            | bits == lobitstring 6 0x09 = ( DcpuRegA, "[B]" )
            | bits == lobitstring 6 0x0a = ( DcpuRegA, "[C]" )
            | bits == lobitstring 6 0x0b = ( DcpuRegA, "[X]" )
            | bits == lobitstring 6 0x0c = ( DcpuRegA, "[Y]" )
            | bits == lobitstring 6 0x0d = ( DcpuRegA, "[Z]" )
            | bits == lobitstring 6 0x0e = ( DcpuRegA, "[I]" )
            | bits == lobitstring 6 0x0f = ( DcpuRegA, "[J]" )
            | bits == lobitstring 6 0x10 = ( DcpuRegA, "[A + next word]" )
            | bits == lobitstring 6 0x11 = ( DcpuRegA, "[B + next word]" )
            | bits == lobitstring 6 0x12 = ( DcpuRegA, "[C + next word]" )
            | bits == lobitstring 6 0x13 = ( DcpuRegA, "[X + next word]" )
            | bits == lobitstring 6 0x14 = ( DcpuRegA, "[Y + next word]" )
            | bits == lobitstring 6 0x15 = ( DcpuRegA, "[Z + next word]" )
            | bits == lobitstring 6 0x16 = ( DcpuRegA, "[I + next word]" )
            | bits == lobitstring 6 0x17 = ( DcpuRegA, "[J + next word]" )
            | bits == lobitstring 6 0x18 = ( DcpuRegA, "(POP / [SP++])" )
            | bits == lobitstring 6 0x19 = ( DcpuRegA, "[SP] / PEEK" )
            | bits == lobitstring 6 0x1a = ( DcpuRegA, "[SP + next word] / PICK n" )
            | bits == lobitstring 6 0x1b = ( DcpuRegA, "SP" )
            | bits == lobitstring 6 0x1c = ( DcpuRegA, "PC" )
            | bits == lobitstring 6 0x1d = ( DcpuRegA, "EX" )
            | bits == lobitstring 6 0x1e = ( DcpuRegA, "[next word]" )
            | bits == lobitstring 6 0x1f = ( DcpuRegA, "next word (literal)" )

            | bits == lobitstring 6 0x20 = ( DcpuRegA, "literal -1" )
            | bits == lobitstring 6 0x21 = ( DcpuRegA, "literal 0" )
            | bits == lobitstring 6 0x22 = ( DcpuRegA, "literal 1" )
            | bits == lobitstring 6 0x23 = ( DcpuRegA, "literal 2" )
            | bits == lobitstring 6 0x24 = ( DcpuRegA, "literal 3" )
            | bits == lobitstring 6 0x25 = ( DcpuRegA, "literal 4" )
            | bits == lobitstring 6 0x26 = ( DcpuRegA, "literal 5" )
            | bits == lobitstring 6 0x27 = ( DcpuRegA, "literal 6" )
            | bits == lobitstring 6 0x28 = ( DcpuRegA, "literal 7" )
            | bits == lobitstring 6 0x29 = ( DcpuRegA, "literal 8" )
            | bits == lobitstring 6 0x2a = ( DcpuRegA, "literal 9" )
            | bits == lobitstring 6 0x2b = ( DcpuRegA, "literal 10" )
            | bits == lobitstring 6 0x2c = ( DcpuRegA, "literal 11" )
            | bits == lobitstring 6 0x2d = ( DcpuRegA, "literal 12" )
            | bits == lobitstring 6 0x2e = ( DcpuRegA, "literal 13" )
            | bits == lobitstring 6 0x2f = ( DcpuRegA, "literal 14" )
            | bits == lobitstring 6 0x30 = ( DcpuRegA, "literal 15" )
            | bits == lobitstring 6 0x31 = ( DcpuRegA, "literal 16" )
            | bits == lobitstring 6 0x32 = ( DcpuRegA, "literal 17" )
            | bits == lobitstring 6 0x33 = ( DcpuRegA, "literal 18" )
            | bits == lobitstring 6 0x34 = ( DcpuRegA, "literal 19" )
            | bits == lobitstring 6 0x35 = ( DcpuRegA, "literal 20" )
            | bits == lobitstring 6 0x36 = ( DcpuRegA, "literal 21" )
            | bits == lobitstring 6 0x37 = ( DcpuRegA, "literal 22" )
            | bits == lobitstring 6 0x38 = ( DcpuRegA, "literal 23" )
            | bits == lobitstring 6 0x39 = ( DcpuRegA, "literal 24" )
            | bits == lobitstring 6 0x3a = ( DcpuRegA, "literal 25" )
            | bits == lobitstring 6 0x3b = ( DcpuRegA, "literal 26" )
            | bits == lobitstring 6 0x3c = ( DcpuRegA, "literal 27" )
            | bits == lobitstring 6 0x3d = ( DcpuRegA, "literal 28" )
            | bits == lobitstring 6 0x3e = ( DcpuRegA, "literal 29" )
            | bits == lobitstring 6 0x3f = ( DcpuRegA, "literal 30" )

-- this is a little complicated for this file
dcpuParser :: Parser [Code.Instruction String (DcpuFieldType, String)]
dcpuParser = many $ instructionparser
    where
        specasts = rights [ specToAst spec label | (spec,label) <- instrspecs ]
        instructionparser = specsToParser convert specasts

-- vim:sw=4:ts=4:et:ai:
