
--
-- DcpuPretty.hs
--

module DcpuPretty (ppinstr) where

import qualified DcpuSpecTables as Dcpu
import CodeAst
import Data.Bit

-- given the code ast for one instruction, return a string that pretty prints it

labelcolumn = 50

bitstostring :: [Bit] -> String
bitstostring = concat . map show

fieldbits :: Field a -> [Bit]
fieldbits (FieldLiteral bs) = bs
fieldbits (FieldVariable _ bs) = bs
fieldbits (FieldNothing) = []

instructionbits :: Instruction a b -> [Bit]
instructionbits (Instruction _ fields) = concat $ map fieldbits fields

-- Dcpu instructions are labeled by strings, so we can set the label type to
-- that here
instructionString :: Show a => Instruction a b -> String
instructionString instr = (bitstostring $ instructionbits instr) ++ (replicate (labelcolumn-(length $ instructionbits instr)) ' ') ++ (show $ label instr) ++ "\n"
    where
        label (Instruction thelabel _) = thelabel

fieldstring :: Show a => Field a -> (String,String)
fieldstring field = ( bitstostring $ fieldbits field, show $ label field )
    where
        label (FieldVariable thelabel _) = thelabel

fieldbitoffsets :: [Field a] -> [Int]
fieldbitoffsets fields = init $ scanl (+) 0 fieldlengths
    where
        fieldlength (FieldLiteral bs) = length bs
        fieldlength (FieldVariable _ bs) = length bs
        fieldlength (FieldNothing) = 0
        fieldlengths = map fieldlength fields

shouldshowfield :: Field a -> Bool
shouldshowfield (FieldLiteral _) = False
shouldshowfield (FieldVariable _ _) = True
shouldshowfield (FieldNothing) = False

ppfieldlist :: Show a => [Field a] -> String
ppfieldlist fieldlist = concat $ map indent tostrings
    where
        zippedfields = zip (fieldbitoffsets fieldlist) fieldlist
        filtered = filter (\(i,f) -> shouldshowfield f) zippedfields
        tostrings = map (\(i,f) -> (i,fieldstring f)) filtered
        indent (i,(bs,label)) = (replicate i ' ') ++ bs ++ (replicate (labelcolumn-i-(length bs)) ' ') ++ label ++ "\n"

-- maybe also put the hex character above each nibble
ppinstr :: Show a => Instruction String a -> String
ppinstr instr = (instructionString instr) ++ (ppfieldlist $ fields instr) ++ "\n"
    where
        fields (Instruction _ thefields) = thefields

-- vim:sw=4:ts=4:et:ai:
