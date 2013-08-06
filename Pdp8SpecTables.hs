
--
-- Pdp8SpecTables.hs
--

module Pdp8SpecTables where

import Data.Bit (Bit)

-- do I even really need a field type?  I can just write out strings for the labels...
data FieldType = Offset | I | Z | Device | Function | Cl | Cm | Rotate | Iac | Cla | Skip_or | Oh | Skip_and | Osr | Hlt
    deriving (Show)

type UserState = ()

instrspecs :: [ ( String, String ) ]
instrspecs = [

        ( "000 IZOffsett", "AND" ),
        ( "001 IZOffsett", "TAD" ),
        ( "010 IZOffsett", "ISZ" ),
        ( "011 IZOffsett", "DCA" ),
        ( "100 IZOffsett", "JMS" ),
        ( "101 IZOffsett", "JMP" ),
        ( "110 DeviceFnc", "IOT" ),

        ( "1110 00 00 000 0", "NOP" ), -- can I rely on this being tried first and overriding the OPR case?
        ( "1110 Cl Cm Rrr C", "OPR (group 1)" ),
        -- need to figure out how to group these bits -- are there any disallowed combinations?

        ( "1111 L Sss 0 Oh 0", "OPR (group 2, or group)" ),

        ( "111 100 001 000", "SKP – Skip Unconditionally" ),
        ( "1111 L Ssx 1 OH 0", "OPR (group 2, and group)" ),

        ( "1111 L Mqx Cod 1", "OPR (group 3)" )

    ]

fieldlabeltable :: [( String, [Bit], (FieldType, String), UserState->UserState )]
fieldlabeltable = [

        ( "Z",       [],        ( Z,        "Z"),                               id ),
        ( "I",       [],        ( I,        "I"),                               id ),
        ( "Offsett", [],        ( Offset,   "offset"),                          id ),
        ( "Device",  [],        ( Device,   "device"),                          id ),
        ( "Fnc",     [],        ( Function, "function"),                        id ),

        -- Group 1
        -- Should these be split?
        ( "Cl",      [0,0],     ( Cl, "---" ),                                  id ),
        ( "Cl",      [1,0],     ( Cl, "CLA: Clear Accumulator" ),               id ),
        ( "Cl",      [0,1],     ( Cl, "CLL: Clear the L Bit" ),                 id ),

        ( "Cm",      [0,1],     ( Cm, "CMA – Ones Complement Accumulator" ),    id ),
        ( "Cm",      [1,0],     ( Cm, "CML – Complement L Bit" ),               id ),

        ( "Rrr",     [0,0,0],   ( Rotate, "---" ),                              id ),
        ( "Rrr",     [1,0,0],   ( Rotate, "RAR: Rotate <L,AC> Right" ),         id ),
        ( "Rrr",     [0,1,0],   ( Rotate, "RAL: Rotate <L,AC> Left" ),          id ),
        ( "Rrr",     [1,0,1],   ( Rotate, "RTR – Rotate <L,AC> Right Twice" ),  id ),
        ( "Rrr",     [0,1,1],   ( Rotate, "RTL – Rotate <L,AC> Left Twice" ),   id ),
        ( "Rrr",     [0,0,1],   ( Rotate, "BSW – Byte Swap 6-bit 'bytes'" ),    id ),

        ( "C",       [0],       ( Iac, "---" ),                                 id ),
        ( "C",       [1],       ( Iac, "IAC – Increment <L,AC>" ),              id ),

        -- Group 2
        ( "L",       [0],       ( Cla, "---" ),                                 id ),
        ( "L",       [1],       ( Cla, "CLA: Clear Accumulator" ),              id ),

        ( "Sss",     [0,0,0],   ( Skip_or, "---" ),                             id ),
        ( "Sss",     [1,0,0],   ( Skip_or, "SMA – Skip on AC < 0 (or group)" ), id ),
        ( "Sss",     [0,1,0],   ( Skip_or, "SZA – Skip on AC = 0 (or group)" ), id ),
        ( "Sss",     [0,0,1],   ( Skip_or, "SNL – Skip on L != 0 (or group)" ), id ),

        -- I think I should split these up... see the pdp 8 quick ref
        ( "Oh",      [0,0],     ( Oh, "---" ),                                  id ),
        ( "Oh",      [1,0],     ( Oh, "OSR – logically 'or' front-panel switches with AC" ), id ),
        ( "Oh",      [0,1],     ( Oh, "HLT" ),                                  id ),

        ( "Ssx",     [0,0,0],   ( Skip_and, "---" ),                               id ),
        ( "Ssx",     [1,0,0],   ( Skip_and, "SPA – Skip on AC >= 0 (and group)" ), id ),
        ( "Ssx",     [0,1,0],   ( Skip_and, "SNA – Skip on AC != 0 (and group)" ), id ),
        ( "Ssx",     [0,0,1],   ( Skip_and, "SZL – Skip on L = 0 (and group)" ),   id ),

        ( "O",       [0],       ( Osr, "---" ),                                 id ),
        ( "O",       [1],       ( Osr, "OSR – logically 'or' front-panel switches with AC" ), id ),

        ( "H",       [0],       ( Hlt, "---" ),                                 id ),
        ( "H",       [1],       ( Hlt, "HLT" ),                                 id )

    ]

-- todo: generate the list of all possible bit strings that this parses, and
-- make sure it matches with all the valid strings that the pdp8 accepts

shouldparsefield :: String -> UserState -> Bool
shouldparsefield _ _ = True

-- vim:sw=4:ts=4:et:ai:
