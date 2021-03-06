
--
-- Pdp8SpecTables.hs
--

module Pdp8SpecTables where

import Data.Bit (Bit)

-- do I even really need a field type?  I can just write out strings for the labels...
data FieldType = Offset | I | Z | Device | Function | Cla | Cll | Cma | Cml | Rotate | Iac | Skip_or | Skip_and | Osr | Hlt | Mqa | Sca | Mql | Code | Immediate
    deriving (Show)

type UserState = ( Bool )

defaultstate :: UserState
defaultstate = (False)

instrspecs :: [ ( String, String ) ]
instrspecs = [

        ( "000 IZOffsett", "AND" ),
        ( "001 IZOffsett", "TAD" ),
        ( "010 IZOffsett", "ISZ" ),
        ( "011 IZOffsett", "DCA" ),
        ( "100 IZOffsett", "JMS" ),
        ( "101 IZOffsett", "JMP" ),
        ( "110 DeviceFnc", "IOT" ),

        -- can I rely on this being tried first and overriding the OPR case?
        ( "1110 00 00 000 0", "NOP" ),
        ( "1110 A L M K Rrr C", "OPR (group 1)" ),

        ( "1111 A Sss 0 O H 0", "OPR (group 2, or group) (reverse sensing bit is 0)" ),
        ( "1111 A Ssx 1 O H 0", "OPR (group 2, and group) (reverse sensing bit is 1)" ),
        -- there is an ambiguity here, should be earlier in the list if it's to
        -- take precedence over the other forms.  Need to confirm that that's
        -- the case during parsing.
        ( "111 100 001 000", "SKP: Skip Unconditionally" ),
        -- consider having an additional "annotate instruction" function or
        -- table that has an additional label for a given bitstring that
        -- describes the whole string and not just the individual fields.
        -- that would handle the NOP and SKP specs above
        --
        -- maybe the label string column could be a function instead?

        -- also: it would be nice to label the reverse sensing bit as a field,
        -- and then have it determine which field to parse (Sss or Ssx).
        -- That's not possible with the setup at the moment.  The userstate
        -- only affects shouldparse.  I suppose you could list out both fields
        -- in a spec and choose one based on the value of the reverse sensing
        -- bit, but that's not very elegant or visually appealing.

        ( "1111 A Q S P Cod 1   Immediatexxx", "OPR (group 3)" )

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
        ( "A",       [0],       ( Cla, "---" ),                                 id ),
        ( "A",       [1],       ( Cla, "CLA: Clear Accumulator" ),              id ),

        ( "L",       [0],       ( Cll, "---" ),                                 id ),
        ( "L",       [1],       ( Cll, "CLL: Clear the L Bit" ),                id ),

        ( "M",       [0],       ( Cma, "---" ),                                 id ),
        ( "M",       [1],       ( Cma, "CMA: Ones Complement Accumulator" ),    id ),

        ( "K",       [0],       ( Cml, "---" ),                                 id ),
        ( "K",       [1],       ( Cml, "CML: Complement L Bit" ),               id ),

        ( "Rrr",     [0,0,0],   ( Rotate, "---" ),                              id ),
        ( "Rrr",     [1,0,0],   ( Rotate, "RAR: Rotate <L,AC> Right" ),         id ),
        ( "Rrr",     [0,1,0],   ( Rotate, "RAL: Rotate <L,AC> Left" ),          id ),
        ( "Rrr",     [1,0,1],   ( Rotate, "RTR: Rotate <L,AC> Right Twice" ),   id ),
        ( "Rrr",     [0,1,1],   ( Rotate, "RTL: Rotate <L,AC> Left Twice" ),    id ),
        ( "Rrr",     [0,0,1],   ( Rotate, "BSW: Byte Swap 6-bit 'bytes'" ),     id ),

        ( "C",       [0],       ( Iac, "---" ),                                 id ),
        ( "C",       [1],       ( Iac, "IAC: Increment <L,AC>" ),               id ),

        -- Group 2
        ( "Sss",     [0,0,0],   ( Skip_or, "---" ),                             id ),
        ( "Sss",     [1,0,0],   ( Skip_or, "SMA: Skip on AC < 0 (or group)" ),  id ),
        ( "Sss",     [0,1,0],   ( Skip_or, "SZA: Skip on AC = 0 (or group)" ),  id ),
        ( "Sss",     [0,0,1],   ( Skip_or, "SNL: Skip on L != 0 (or group)" ),  id ),

        ( "Ssx",     [0,0,0],   ( Skip_and, "---" ),                              id ),
        ( "Ssx",     [1,0,0],   ( Skip_and, "SPA: Skip on AC >= 0 (and group)" ), id ),
        ( "Ssx",     [0,1,0],   ( Skip_and, "SNA: Skip on AC != 0 (and group)" ), id ),
        ( "Ssx",     [0,0,1],   ( Skip_and, "SZL: Skip on L = 0 (and group)" ),   id ),

        ( "O",       [0],       ( Osr, "---" ),                                 id ),
        ( "O",       [1],       ( Osr, "OSR: logically 'or' front-panel switches with AC" ), id ),

        ( "H",       [0],       ( Hlt, "---" ),                                 id ),
        ( "H",       [1],       ( Hlt, "HLT" ),                                 id ),

        -- Group 3
        ( "Q",       [0],       ( Mqa, "---" ),                                 id ),
        ( "Q",       [1],       ( Mqa, "MQA: Multiplier Quotient with AC" ),    id ),

        ( "S",       [0],       ( Sca, "---" ),                                 id ),
        ( "S",       [1],       ( Sca, "SCA: Step counter load into AC" ),      id ),

        ( "P",       [0],       ( Mql, "---" ),                                 id ),
        ( "P",       [1],       ( Mql, "MQL: Multiplier Quotient Load" ),       id ),

        ( "Cod",     [0,0,0],   ( Code, "No operation" ),                       id ),
        ( "Cod",     [0,0,1],   ( Code, "SCL: Step Counter Load (immediate word follows)" ), parseimmediate ),
        ( "Cod",     [0,1,0],   ( Code, "MUY: Multiply" ),                      id ),
        ( "Cod",     [0,1,1],   ( Code, "DVI: Divide" ),                        id ),
        ( "Cod",     [1,0,0],   ( Code, "NMI: Normalize" ),                     id ),
        ( "Cod",     [1,0,1],   ( Code, "SHL: Shift left (immediate word follows)" ), parseimmediate ),
        ( "Cod",     [1,1,0],   ( Code, "ASR: Arithmetic shift right" ),        id ),
        ( "Cod",     [1,1,1],   ( Code, "LSR: Logical shift right " ),          id ),

        ( "Immediatexxx", [],   ( Immediate, "immediate value" ),               id )

    ]
    where
        parseimmediate = \_ -> ( True )

-- todo: generate the list of all possible bit strings that this parses, and
-- make sure it matches with all the valid strings that the pdp8 accepts

shouldparsefield :: String -> UserState -> Bool
shouldparsefield "Immediatexxx" (a) = a
shouldparsefield _ _ = True

-- vim:sw=4:ts=4:et:ai:
