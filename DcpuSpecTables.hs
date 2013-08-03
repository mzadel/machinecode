
--
-- DcpuSpecTables.hs
--

module DcpuSpecTables where

import BitList (bitsFromByte)
import Data.Bit (Bit)

data FieldType = RegA | RegB | OptionalWord
    deriving (Show)

type UserState = ( Bool )

-- Compute a string of ones and zeroes from the byte value.
-- (Using this so the table here matches the dcpu spec document.)
toString = concat . map show . drop 3 . bitsFromByte

instrspecs :: [ ( String, String ) ]
instrspecs = [

        ( "AaaaaaBbbbb" ++ toString 0x01, "SET b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x02, "ADD b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x03, "SUB b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x04, "MUL b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x05, "MLI b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x06, "DIV b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x07, "DVI b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x08, "MOD b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x09, "MDI b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x0a, "AND b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x0b, "BOR b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x0c, "XOR b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x0d, "SHR b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x0e, "ASR b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x0f, "SHL b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x10, "IFB b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x11, "IFC b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x12, "IFE b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x13, "IFN b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x14, "IFG b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x15, "IFA b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x16, "IFL b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x17, "IFU b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x1a, "ADX b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x1b, "SBX b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x1e, "STI b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x1f, "STD b, a" ),

        ( "Aaaaaa" ++ toString 0x01 ++ " 00000", "JSR a" ),
        ( "Aaaaaa" ++ toString 0x08 ++ " 00000", "INT a" ),
        ( "Aaaaaa" ++ toString 0x09 ++ " 00000", "IAG a" ),
        ( "Aaaaaa" ++ toString 0x0a ++ " 00000", "IAS a" ),
        ( "Aaaaaa" ++ toString 0x0b ++ " 00000", "RFI a" ),
        ( "Aaaaaa" ++ toString 0x0c ++ " 00000", "IAQ a" ),
        ( "Aaaaaa" ++ toString 0x10 ++ " 00000", "HWN a" ),
        ( "Aaaaaa" ++ toString 0x11 ++ " 00000", "HWQ a" ),
        ( "Aaaaaa" ++ toString 0x12 ++ " 00000", "HWI a" )

    ]

-- Compute a bit list from an int value.  I'm expressing these in hex values
-- so they match the dcpu instruction specification, so they'll be easier to
-- compare.
toRegABits = drop 2 . bitsFromByte
toRegBBits = drop 3 . bitsFromByte


-- here I'll need to have: should I parse this?  what's the state transformation when I do?
-- columns should be:
-- ( String, [Bit], shouldparse, (FieldType, String), statetransform ), ie
-- ( String, [Bit], UserState->Bool, (FieldType, String), UserState->UserState )
fieldlabeltable :: [( String, [Bit], (FieldType, String), UserState->UserState )]
fieldlabeltable = [

        ( "Aaaaaa", toRegABits 0x00, ( RegA, "A" ),                            id ),
        ( "Aaaaaa", toRegABits 0x01, ( RegA, "B" ),                            id ),
        ( "Aaaaaa", toRegABits 0x02, ( RegA, "C" ),                            id ),
        ( "Aaaaaa", toRegABits 0x03, ( RegA, "X" ),                            id ),
        ( "Aaaaaa", toRegABits 0x04, ( RegA, "Y" ),                            id ),
        ( "Aaaaaa", toRegABits 0x05, ( RegA, "Z" ),                            id ),
        ( "Aaaaaa", toRegABits 0x06, ( RegA, "I" ),                            id ),
        ( "Aaaaaa", toRegABits 0x07, ( RegA, "J" ),                            id ),
        ( "Aaaaaa", toRegABits 0x08, ( RegA, "[A]" ),                          id ),
        ( "Aaaaaa", toRegABits 0x09, ( RegA, "[B]" ),                          id ),
        ( "Aaaaaa", toRegABits 0x0a, ( RegA, "[C]" ),                          id ),
        ( "Aaaaaa", toRegABits 0x0b, ( RegA, "[X]" ),                          id ),
        ( "Aaaaaa", toRegABits 0x0c, ( RegA, "[Y]" ),                          id ),
        ( "Aaaaaa", toRegABits 0x0d, ( RegA, "[Z]" ),                          id ),
        ( "Aaaaaa", toRegABits 0x0e, ( RegA, "[I]" ),                          id ),
        ( "Aaaaaa", toRegABits 0x0f, ( RegA, "[J]" ),                          id ),
        ( "Aaaaaa", toRegABits 0x10, ( RegA, "[A + next word]" ),              parsenextword ),
        ( "Aaaaaa", toRegABits 0x11, ( RegA, "[B + next word]" ),              parsenextword ),
        ( "Aaaaaa", toRegABits 0x12, ( RegA, "[C + next word]" ),              parsenextword ),
        ( "Aaaaaa", toRegABits 0x13, ( RegA, "[X + next word]" ),              parsenextword ),
        ( "Aaaaaa", toRegABits 0x14, ( RegA, "[Y + next word]" ),              parsenextword ),
        ( "Aaaaaa", toRegABits 0x15, ( RegA, "[Z + next word]" ),              parsenextword ),
        ( "Aaaaaa", toRegABits 0x16, ( RegA, "[I + next word]" ),              parsenextword ),
        ( "Aaaaaa", toRegABits 0x17, ( RegA, "[J + next word]" ),              parsenextword ),
        ( "Aaaaaa", toRegABits 0x18, ( RegA, "(POP / [SP++])" ),               id ),
        ( "Aaaaaa", toRegABits 0x19, ( RegA, "[SP] / PEEK" ),                  id ),
        ( "Aaaaaa", toRegABits 0x1a, ( RegA, "[SP + next word] / PICK n" ),    parsenextword ),
        ( "Aaaaaa", toRegABits 0x1b, ( RegA, "SP" ),                           id ),
        ( "Aaaaaa", toRegABits 0x1c, ( RegA, "PC" ),                           id ),
        ( "Aaaaaa", toRegABits 0x1d, ( RegA, "EX" ),                           id ),
        ( "Aaaaaa", toRegABits 0x1e, ( RegA, "[next word]" ),                  parsenextword ),
        ( "Aaaaaa", toRegABits 0x1f, ( RegA, "next word (literal)" ),          parsenextword ),

        ( "Aaaaaa", toRegABits 0x20, ( RegA, "literal -1" ),                   id ),
        ( "Aaaaaa", toRegABits 0x21, ( RegA, "literal 0" ),                    id ),
        ( "Aaaaaa", toRegABits 0x22, ( RegA, "literal 1" ),                    id ),
        ( "Aaaaaa", toRegABits 0x23, ( RegA, "literal 2" ),                    id ),
        ( "Aaaaaa", toRegABits 0x24, ( RegA, "literal 3" ),                    id ),
        ( "Aaaaaa", toRegABits 0x25, ( RegA, "literal 4" ),                    id ),
        ( "Aaaaaa", toRegABits 0x26, ( RegA, "literal 5" ),                    id ),
        ( "Aaaaaa", toRegABits 0x27, ( RegA, "literal 6" ),                    id ),
        ( "Aaaaaa", toRegABits 0x28, ( RegA, "literal 7" ),                    id ),
        ( "Aaaaaa", toRegABits 0x29, ( RegA, "literal 8" ),                    id ),
        ( "Aaaaaa", toRegABits 0x2a, ( RegA, "literal 9" ),                    id ),
        ( "Aaaaaa", toRegABits 0x2b, ( RegA, "literal 10" ),                   id ),
        ( "Aaaaaa", toRegABits 0x2c, ( RegA, "literal 11" ),                   id ),
        ( "Aaaaaa", toRegABits 0x2d, ( RegA, "literal 12" ),                   id ),
        ( "Aaaaaa", toRegABits 0x2e, ( RegA, "literal 13" ),                   id ),
        ( "Aaaaaa", toRegABits 0x2f, ( RegA, "literal 14" ),                   id ),
        ( "Aaaaaa", toRegABits 0x30, ( RegA, "literal 15" ),                   id ),
        ( "Aaaaaa", toRegABits 0x31, ( RegA, "literal 16" ),                   id ),
        ( "Aaaaaa", toRegABits 0x32, ( RegA, "literal 17" ),                   id ),
        ( "Aaaaaa", toRegABits 0x33, ( RegA, "literal 18" ),                   id ),
        ( "Aaaaaa", toRegABits 0x34, ( RegA, "literal 19" ),                   id ),
        ( "Aaaaaa", toRegABits 0x35, ( RegA, "literal 20" ),                   id ),
        ( "Aaaaaa", toRegABits 0x36, ( RegA, "literal 21" ),                   id ),
        ( "Aaaaaa", toRegABits 0x37, ( RegA, "literal 22" ),                   id ),
        ( "Aaaaaa", toRegABits 0x38, ( RegA, "literal 23" ),                   id ),
        ( "Aaaaaa", toRegABits 0x39, ( RegA, "literal 24" ),                   id ),
        ( "Aaaaaa", toRegABits 0x3a, ( RegA, "literal 25" ),                   id ),
        ( "Aaaaaa", toRegABits 0x3b, ( RegA, "literal 26" ),                   id ),
        ( "Aaaaaa", toRegABits 0x3c, ( RegA, "literal 27" ),                   id ),
        ( "Aaaaaa", toRegABits 0x3d, ( RegA, "literal 28" ),                   id ),
        ( "Aaaaaa", toRegABits 0x3e, ( RegA, "literal 29" ),                   id ),
        ( "Aaaaaa", toRegABits 0x3f, ( RegA, "literal 30" ),                   id ),

        ( "Bbbbb",  toRegBBits 0x00, ( RegB, "A" ),                            id ),
        ( "Bbbbb",  toRegBBits 0x01, ( RegB, "B" ),                            id ),
        ( "Bbbbb",  toRegBBits 0x02, ( RegB, "C" ),                            id ),
        ( "Bbbbb",  toRegBBits 0x03, ( RegB, "X" ),                            id ),
        ( "Bbbbb",  toRegBBits 0x04, ( RegB, "Y" ),                            id ),
        ( "Bbbbb",  toRegBBits 0x05, ( RegB, "Z" ),                            id ),
        ( "Bbbbb",  toRegBBits 0x06, ( RegB, "I" ),                            id ),
        ( "Bbbbb",  toRegBBits 0x07, ( RegB, "J" ),                            id ),
        ( "Bbbbb",  toRegBBits 0x08, ( RegB, "[A]" ),                          id ),
        ( "Bbbbb",  toRegBBits 0x09, ( RegB, "[B]" ),                          id ),
        ( "Bbbbb",  toRegBBits 0x0a, ( RegB, "[C]" ),                          id ),
        ( "Bbbbb",  toRegBBits 0x0b, ( RegB, "[X]" ),                          id ),
        ( "Bbbbb",  toRegBBits 0x0c, ( RegB, "[Y]" ),                          id ),
        ( "Bbbbb",  toRegBBits 0x0d, ( RegB, "[Z]" ),                          id ),
        ( "Bbbbb",  toRegBBits 0x0e, ( RegB, "[I]" ),                          id ),
        ( "Bbbbb",  toRegBBits 0x0f, ( RegB, "[J]" ),                          id ),
        ( "Bbbbb",  toRegBBits 0x10, ( RegB, "[A + next word]" ),              parsenextword ),
        ( "Bbbbb",  toRegBBits 0x11, ( RegB, "[B + next word]" ),              parsenextword ),
        ( "Bbbbb",  toRegBBits 0x12, ( RegB, "[C + next word]" ),              parsenextword ),
        ( "Bbbbb",  toRegBBits 0x13, ( RegB, "[X + next word]" ),              parsenextword ),
        ( "Bbbbb",  toRegBBits 0x14, ( RegB, "[Y + next word]" ),              parsenextword ),
        ( "Bbbbb",  toRegBBits 0x15, ( RegB, "[Z + next word]" ),              parsenextword ),
        ( "Bbbbb",  toRegBBits 0x16, ( RegB, "[I + next word]" ),              parsenextword ),
        ( "Bbbbb",  toRegBBits 0x17, ( RegB, "[J + next word]" ),              parsenextword ),
        ( "Bbbbb",  toRegBBits 0x18, ( RegB, "(PUSH / [--SP])" ),              id ),
        ( "Bbbbb",  toRegBBits 0x19, ( RegB, "[SP] / PEEK" ),                  id ),
        ( "Bbbbb",  toRegBBits 0x1a, ( RegB, "[SP + next word] / PICK n" ),    parsenextword ),
        ( "Bbbbb",  toRegBBits 0x1b, ( RegB, "SP" ),                           id ),
        ( "Bbbbb",  toRegBBits 0x1c, ( RegB, "PC" ),                           id ),
        ( "Bbbbb",  toRegBBits 0x1d, ( RegB, "EX" ),                           id ),
        ( "Bbbbb",  toRegBBits 0x1e, ( RegB, "[next word]" ),                  parsenextword ),
        ( "Bbbbb",  toRegBBits 0x1f, ( RegB, "next word (literal)" ),          parsenextword ),

        ( "Dddddddddddddddd", [], ( OptionalWord, "value "), id )

    ]
    where
        parsenextword = \_ -> (True)

-- collect these "should parse?" judgements in one separate table since it
-- should be per-field-type I think
shouldparse :: String -> UserState -> Bool
shouldparse "Aaaaaa" _ = True
shouldparse "Bbbbb" _ = True
shouldparse "Dddddddddddddddd" (flag) = flag



-- vim:sw=4:ts=4:et:ai:
