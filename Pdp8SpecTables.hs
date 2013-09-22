
--
-- Pdp8SpecTables.hs
--

module Pdp8SpecTables where

import Data.Bit (Bit)

-- do I even really need a field type?  I can just write out strings for the labels...
data FieldType = Offset | I | Z | Device | Function | Cla | Cll | Cma | Cml | Rotate | Iac | Skip_or | Skip_and | Osr | Hlt
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
        ( "1110 AL MK Rrr C", "OPR (group 1)" ),
        -- need to figure out how to group these bits -- are there any disallowed combinations?

        ( "1111 A Sss 0 OH 0", "OPR (group 2, or group)" ),

        ( "111 100 001 000", "SKP – Skip Unconditionally" ),
        ( "1111 A Ssx 1 OH 0", "OPR (group 2, and group)" ),

        ( "1111 A Mqx Cod 1", "OPR (group 3)" )

    ]

fieldlabeltable :: [( String, [Bit], (FieldType, String), UserState->UserState )]
fieldlabeltable = [

        ( "Z",       [0],       ( Z,        "Page selector: page 0"),           id ),
        ( "Z",       [1],       ( Z,        "Page selector: current page"),     id ),
        ( "I",       [0],       ( I,        "Address mode: direct"),            id ),
        ( "I",       [1],       ( I,        "Address mode: indirect"),          id ),
        ( "Offsett", [],        ( Offset,   "offset"),                          id ),
        ( "Device",  [],        ( Device,   "device"),                          id ),
        ( "Fnc",     [],        ( Function, "function"),                        id ),

        -- Group 1
        ( "A",       [0],       ( CLA, "---" ),                                 id ),
        ( "A",       [1]        ( CLA, "CLA: Clear Accumulator" ),              id ),

        ( "L",       [0],       ( CLL, "---" ),                                 id ),
        ( "L",       [1],       ( CLL, "CLL: Clear the L Bit" ),                id ),

        ( "M",       [0],       ( CMA, "---" ),                                 id ),
        ( "M",       [1],       ( CMA, "CMA – Ones Complement Accumulator" ),   id ),

        ( "K",       [0],       ( CML, "---" ),                                 id ),
        ( "K",       [1],       ( CML, "CML – Complement L Bit" ),              id ),

        ( "Rrr",     [0,0,0],   ( Rotate, "---" ),                              id ),
        ( "Rrr",     [1,0,0],   ( Rotate, "RAR: Rotate <L,AC> Right" ),         id ),
        ( "Rrr",     [0,1,0],   ( Rotate, "RAL: Rotate <L,AC> Left" ),          id ),
        ( "Rrr",     [1,0,1],   ( Rotate, "RTR – Rotate <L,AC> Right Twice" ),  id ),
        ( "Rrr",     [0,1,1],   ( Rotate, "RTL – Rotate <L,AC> Left Twice" ),   id ),
        ( "Rrr",     [0,0,1],   ( Rotate, "BSW – Byte Swap 6-bit 'bytes'" ),    id ),

        ( "C",       [0],       ( Iac, "---" ),                                 id ),
        ( "C",       [1],       ( Iac, "IAC – Increment <L,AC>" ),              id ),

        -- Group 2
        ( "Sss",     [0,0,0],   ( Skip_or, "---" ),                             id ),
        ( "Sss",     [1,0,0],   ( Skip_or, "SMA – Skip on AC < 0 (or group)" ), id ),
        ( "Sss",     [0,1,0],   ( Skip_or, "SZA – Skip on AC = 0 (or group)" ), id ),
        ( "Sss",     [0,0,1],   ( Skip_or, "SNL – Skip on L != 0 (or group)" ), id ),

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
