
--
-- Pretty.hs
--

module Pretty where

import CodeAst
import Data.Bit
import Data.Word (Word16)
import Data.Bits
import BitList (bitstostring,bitsToWord16)

-- given the code ast for one instruction, return a string that pretty prints it
-- todo: split out the generic parts of this from the dcpu-specific parts

labelcolumn = 50

tohex :: [Bit] -> String
tohex bs = concat $ map onechar (byfours bs)
    where
        byfours :: [Bit] -> [[Bit]]
        byfours [] = []
        byfours bs = a : byfours b
            where
                (a,b) = splitAt 4 bs
        onechar (0:0:0:0:bs) = "0   "
        onechar (0:0:0:1:bs) = "1   "
        onechar (0:0:1:0:bs) = "2   "
        onechar (0:0:1:1:bs) = "3   "
        onechar (0:1:0:0:bs) = "4   "
        onechar (0:1:0:1:bs) = "5   "
        onechar (0:1:1:0:bs) = "6   "
        onechar (0:1:1:1:bs) = "7   "
        onechar (1:0:0:0:bs) = "8   "
        onechar (1:0:0:1:bs) = "9   "
        onechar (1:0:1:0:bs) = "a   "
        onechar (1:0:1:1:bs) = "b   "
        onechar (1:1:0:0:bs) = "c   "
        onechar (1:1:0:1:bs) = "d   "
        onechar (1:1:1:0:bs) = "e   "
        onechar (1:1:1:1:bs) = "f   "

fieldbits :: Field a -> [Bit]
fieldbits (FieldLiteral bs) = bs
fieldbits (FieldVariable _ bs) = bs
fieldbits (FieldNothing) = []

instructionbits :: Instruction a b -> [Bit]
instructionbits (Instruction _ fields) = concat $ map fieldbits fields

instructionString :: Show a => Instruction a b -> String
instructionString instr = (bitstostring $ instructionbits instr) ++ (replicate (labelcolumn-(length $ instructionbits instr)) ' ') ++ (show $ label instr) ++ "\n"
    where
        label (Instruction thelabel _) = thelabel

fieldstring :: Show a => (Field a -> String) -> Field a -> (String,String)
fieldstring labeltostring field = ( bitstostring $ fieldbits field, labeltostring field )

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

ppfieldlist :: Show a => (Field a -> String) -> [Field a] -> String
ppfieldlist labeltostring fieldlist = concat $ map indent tostrings
    where
        zippedfields = zip (fieldbitoffsets fieldlist) fieldlist
        filtered = filter (\(i,f) -> shouldshowfield f) zippedfields
        tostrings = map (\(i,f) -> (i,fieldstring labeltostring f)) filtered
        indent (i,(bs,label)) = (replicate i ' ') ++ bs ++ (replicate (labelcolumn-i-(length bs)) ' ') ++ label ++ "\n"

ppinstr :: Show a => (Field a -> String) -> Instruction String a -> String
ppinstr labeltostring instr = (tohex $ instructionbits instr) ++ "\n" ++ (instructionString instr) ++ (ppfieldlist labeltostring $ fields instr) ++ "\n"
    where
        fields (Instruction _ thefields) = thefields

-- vim:sw=4:ts=4:et:ai:
