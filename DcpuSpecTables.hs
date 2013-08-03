
--
-- DcpuSpecTables.hs
--

module DcpuSpecTables where

import BitList (bitsFromByte)
import Data.Bit (Bit)

data FieldType = RegA | RegB | RegADataWord | RegBDataWord
    deriving (Show)

-- UserState is the internal state of the code parser as it's going through an
-- instruction.  For dcpu, the flag values are
-- (parse next word for reg A, parse next word for reg B)
type UserState = ( Bool, Bool )

-- the default state that is used at the beginning of each instruction
defaultstate :: UserState
defaultstate = (False,False)

-- Compute a string of ones and zeroes from the byte value.
-- (Using this so the table here matches the dcpu spec document.)
toString = concat . map show . drop 3 . bitsFromByte

instrspecs :: [ ( String, String ) ]
instrspecs = [

        ( "AaaaaaBbbbb" ++ toString 0x01 ++ " Regadatawordxxxx Regbdatawordxxxx", "SET b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x02 ++ " Regadatawordxxxx Regbdatawordxxxx", "ADD b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x03 ++ " Regadatawordxxxx Regbdatawordxxxx", "SUB b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x04 ++ " Regadatawordxxxx Regbdatawordxxxx", "MUL b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x05 ++ " Regadatawordxxxx Regbdatawordxxxx", "MLI b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x06 ++ " Regadatawordxxxx Regbdatawordxxxx", "DIV b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x07 ++ " Regadatawordxxxx Regbdatawordxxxx", "DVI b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x08 ++ " Regadatawordxxxx Regbdatawordxxxx", "MOD b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x09 ++ " Regadatawordxxxx Regbdatawordxxxx", "MDI b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x0a ++ " Regadatawordxxxx Regbdatawordxxxx", "AND b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x0b ++ " Regadatawordxxxx Regbdatawordxxxx", "BOR b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x0c ++ " Regadatawordxxxx Regbdatawordxxxx", "XOR b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x0d ++ " Regadatawordxxxx Regbdatawordxxxx", "SHR b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x0e ++ " Regadatawordxxxx Regbdatawordxxxx", "ASR b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x0f ++ " Regadatawordxxxx Regbdatawordxxxx", "SHL b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x10 ++ " Regadatawordxxxx Regbdatawordxxxx", "IFB b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x11 ++ " Regadatawordxxxx Regbdatawordxxxx", "IFC b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x12 ++ " Regadatawordxxxx Regbdatawordxxxx", "IFE b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x13 ++ " Regadatawordxxxx Regbdatawordxxxx", "IFN b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x14 ++ " Regadatawordxxxx Regbdatawordxxxx", "IFG b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x15 ++ " Regadatawordxxxx Regbdatawordxxxx", "IFA b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x16 ++ " Regadatawordxxxx Regbdatawordxxxx", "IFL b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x17 ++ " Regadatawordxxxx Regbdatawordxxxx", "IFU b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x1a ++ " Regadatawordxxxx Regbdatawordxxxx", "ADX b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x1b ++ " Regadatawordxxxx Regbdatawordxxxx", "SBX b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x1e ++ " Regadatawordxxxx Regbdatawordxxxx", "STI b, a" ),
        ( "AaaaaaBbbbb" ++ toString 0x1f ++ " Regadatawordxxxx Regbdatawordxxxx", "STD b, a" ),

        ( "Aaaaaa" ++ toString 0x01 ++ " 00000 Regadatawordxxxx", "JSR a" ),
        ( "Aaaaaa" ++ toString 0x08 ++ " 00000 Regadatawordxxxx", "INT a" ),
        ( "Aaaaaa" ++ toString 0x09 ++ " 00000 Regadatawordxxxx", "IAG a" ),
        ( "Aaaaaa" ++ toString 0x0a ++ " 00000 Regadatawordxxxx", "IAS a" ),
        ( "Aaaaaa" ++ toString 0x0b ++ " 00000 Regadatawordxxxx", "RFI a" ),
        ( "Aaaaaa" ++ toString 0x0c ++ " 00000 Regadatawordxxxx", "IAQ a" ),
        ( "Aaaaaa" ++ toString 0x10 ++ " 00000 Regadatawordxxxx", "HWN a" ),
        ( "Aaaaaa" ++ toString 0x11 ++ " 00000 Regadatawordxxxx", "HWQ a" ),
        ( "Aaaaaa" ++ toString 0x12 ++ " 00000 Regadatawordxxxx", "HWI a" )

    ]

-- Compute a bit list from an int value.  I'm expressing these in hex values
-- so they match the dcpu instruction specification, so they'll be easier to
-- compare.
toRegABits = drop 2 . bitsFromByte
toRegBBits = drop 3 . bitsFromByte


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
        ( "Aaaaaa", toRegABits 0x10, ( RegA, "[A + next word]" ),              parsenextwordA ),
        ( "Aaaaaa", toRegABits 0x11, ( RegA, "[B + next word]" ),              parsenextwordA ),
        ( "Aaaaaa", toRegABits 0x12, ( RegA, "[C + next word]" ),              parsenextwordA ),
        ( "Aaaaaa", toRegABits 0x13, ( RegA, "[X + next word]" ),              parsenextwordA ),
        ( "Aaaaaa", toRegABits 0x14, ( RegA, "[Y + next word]" ),              parsenextwordA ),
        ( "Aaaaaa", toRegABits 0x15, ( RegA, "[Z + next word]" ),              parsenextwordA ),
        ( "Aaaaaa", toRegABits 0x16, ( RegA, "[I + next word]" ),              parsenextwordA ),
        ( "Aaaaaa", toRegABits 0x17, ( RegA, "[J + next word]" ),              parsenextwordA ),
        ( "Aaaaaa", toRegABits 0x18, ( RegA, "(POP / [SP++])" ),               id ),
        ( "Aaaaaa", toRegABits 0x19, ( RegA, "[SP] / PEEK" ),                  id ),
        ( "Aaaaaa", toRegABits 0x1a, ( RegA, "[SP + next word] / PICK n" ),    parsenextwordA ),
        ( "Aaaaaa", toRegABits 0x1b, ( RegA, "SP" ),                           id ),
        ( "Aaaaaa", toRegABits 0x1c, ( RegA, "PC" ),                           id ),
        ( "Aaaaaa", toRegABits 0x1d, ( RegA, "EX" ),                           id ),
        ( "Aaaaaa", toRegABits 0x1e, ( RegA, "[next word]" ),                  parsenextwordA ),
        ( "Aaaaaa", toRegABits 0x1f, ( RegA, "next word (literal)" ),          parsenextwordA ),

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
        ( "Bbbbb",  toRegBBits 0x10, ( RegB, "[A + next word]" ),              parsenextwordB ),
        ( "Bbbbb",  toRegBBits 0x11, ( RegB, "[B + next word]" ),              parsenextwordB ),
        ( "Bbbbb",  toRegBBits 0x12, ( RegB, "[C + next word]" ),              parsenextwordB ),
        ( "Bbbbb",  toRegBBits 0x13, ( RegB, "[X + next word]" ),              parsenextwordB ),
        ( "Bbbbb",  toRegBBits 0x14, ( RegB, "[Y + next word]" ),              parsenextwordB ),
        ( "Bbbbb",  toRegBBits 0x15, ( RegB, "[Z + next word]" ),              parsenextwordB ),
        ( "Bbbbb",  toRegBBits 0x16, ( RegB, "[I + next word]" ),              parsenextwordB ),
        ( "Bbbbb",  toRegBBits 0x17, ( RegB, "[J + next word]" ),              parsenextwordB ),
        ( "Bbbbb",  toRegBBits 0x18, ( RegB, "(PUSH / [--SP])" ),              id ),
        ( "Bbbbb",  toRegBBits 0x19, ( RegB, "[SP] / PEEK" ),                  id ),
        ( "Bbbbb",  toRegBBits 0x1a, ( RegB, "[SP + next word] / PICK n" ),    parsenextwordB ),
        ( "Bbbbb",  toRegBBits 0x1b, ( RegB, "SP" ),                           id ),
        ( "Bbbbb",  toRegBBits 0x1c, ( RegB, "PC" ),                           id ),
        ( "Bbbbb",  toRegBBits 0x1d, ( RegB, "EX" ),                           id ),
        ( "Bbbbb",  toRegBBits 0x1e, ( RegB, "[next word]" ),                  parsenextwordB ),
        ( "Bbbbb",  toRegBBits 0x1f, ( RegB, "next word (literal)" ),          parsenextwordB ),

        ( "Regadatawordxxxx", [], ( RegADataWord, "next word"), id ),
        ( "Regbdatawordxxxx", [], ( RegBDataWord, "next word"), id )

    ]
    where
        -- set the flag in the user state to true to indicate that it should
        -- expect the Regadatawordxxxx or Regbdatawordxxxx
        parsenextwordA = \(a,b) -> (True,b)
        parsenextwordB = \(a,b) -> (a,True)

-- answer true if a field with this spec should be parsed given the current
-- state
shouldparsefield :: String -> UserState -> Bool
shouldparsefield "Aaaaaa" _ = True
shouldparsefield "Bbbbb" _ = True
shouldparsefield "Regadatawordxxxx" (a,b) = a
shouldparsefield "Regbdatawordxxxx" (a,b) = b



-- vim:sw=4:ts=4:et:ai:
