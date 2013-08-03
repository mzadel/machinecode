
--
-- DcpuPretty.hs
--

module DcpuPretty (ppinstr) where

import qualified DcpuSpecTables as Dcpu
import CodeAst
import Data.Bit
import Data.Word (Word16)
import Data.Bits

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

bitsToWord16 :: [Bit] -> Word16
bitsToWord16 [b15,b14,b13,b12,b11,b10,b9,b8,b7,b6,b5,b4,b3,b2,b1,b0] =
    fromIntegral b15 * 0x8000 +
    fromIntegral b14 * 0x4000 +
    fromIntegral b13 * 0x2000 +
    fromIntegral b12 * 0x1000 +
    fromIntegral b11 * 0x0800 +
    fromIntegral b10 * 0x0400 +
    fromIntegral b9  * 0x0200 +
    fromIntegral b8  * 0x0100 +
    fromIntegral b7  * 0x0080 +
    fromIntegral b6  * 0x0040 +
    fromIntegral b5  * 0x0020 +
    fromIntegral b4  * 0x0010 +
    fromIntegral b3  * 0x0008 +
    fromIntegral b2  * 0x0004 +
    fromIntegral b1  * 0x0002 +
    fromIntegral b0  * 0x0001

labeltostring :: Show a => Field (Dcpu.FieldType, a) -> String
labeltostring (FieldLiteral _) = ""
labeltostring (FieldNothing) = ""
labeltostring (FieldVariable (Dcpu.RegA,label) bs) = "Register A: " ++ show label
labeltostring (FieldVariable (Dcpu.RegB,label) bs) = "Register B: " ++ show label
labeltostring (FieldVariable (Dcpu.RegADataWord,label) bs) = "Register A data: " ++ bitstostring bs ++ " (" ++ (show $ bitsToWord16 bs) ++ ")"
labeltostring (FieldVariable (Dcpu.RegBDataWord,label) bs) = "Register B data: " ++ bitstostring bs ++ " (" ++ (show $ bitsToWord16 bs) ++ ")"

fieldstring :: Show a => Field (Dcpu.FieldType, a) -> (String,String)
fieldstring field = ( bitstostring $ fieldbits field, labeltostring field )

fieldbitoffsets :: [Field (Dcpu.FieldType, a)] -> [Int]
fieldbitoffsets fields = init $ scanl (+) 0 fieldlengths
    where
        fieldlength (FieldLiteral bs) = length bs
        fieldlength (FieldVariable _ bs) = length bs
        fieldlength (FieldNothing) = 0
        fieldlengths = map fieldlength fields

shouldshowfield :: Field (Dcpu.FieldType, a) -> Bool
shouldshowfield (FieldLiteral _) = False
shouldshowfield (FieldVariable _ _) = True
shouldshowfield (FieldNothing) = False

ppfieldlist :: Show a => [Field (Dcpu.FieldType, a)] -> String
ppfieldlist fieldlist = concat $ map indent tostrings
    where
        zippedfields = zip (fieldbitoffsets fieldlist) fieldlist
        filtered = filter (\(i,f) -> shouldshowfield f) zippedfields
        tostrings = map (\(i,f) -> (i,fieldstring f)) filtered
        indent (i,(bs,label)) = (replicate i ' ') ++ bs ++ (replicate (labelcolumn-i-(length bs)) ' ') ++ label ++ "\n"

-- maybe also put the hex character above each nibble
ppinstr :: Show a => Instruction String (Dcpu.FieldType, a) -> String
ppinstr instr = (instructionString instr) ++ (ppfieldlist $ fields instr) ++ "\n"
    where
        fields (Instruction _ thefields) = thefields

-- vim:sw=4:ts=4:et:ai:
