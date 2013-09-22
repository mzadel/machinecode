
--
-- Pretty.hs
--

module Pretty (printInstruction) where

import CodeAst
import Data.Bit
import BitList (bitstostring)

-- given the code ast for one instruction, return a string that pretty prints it
-- todo: split out the generic parts of this from the dcpu-specific parts

labelcolumn :: Int
labelcolumn = 50

tohex :: [Bit] -> String
tohex bs = concat $ map onechar (byfours bs)
    where
        byfours :: [Bit] -> [[Bit]]
        byfours [] = []
        byfours bitlist = a : byfours b
            where
                (a,b) = splitAt 4 bitlist
        onechar (0:0:0:0:_) = "0   "
        onechar (0:0:0:1:_) = "1   "
        onechar (0:0:1:0:_) = "2   "
        onechar (0:0:1:1:_) = "3   "
        onechar (0:1:0:0:_) = "4   "
        onechar (0:1:0:1:_) = "5   "
        onechar (0:1:1:0:_) = "6   "
        onechar (0:1:1:1:_) = "7   "
        onechar (1:0:0:0:_) = "8   "
        onechar (1:0:0:1:_) = "9   "
        onechar (1:0:1:0:_) = "a   "
        onechar (1:0:1:1:_) = "b   "
        onechar (1:1:0:0:_) = "c   "
        onechar (1:1:0:1:_) = "d   "
        onechar (1:1:1:0:_) = "e   "
        onechar (1:1:1:1:_) = "f   "

tooctal :: [Bit] -> String
tooctal bs = concat $ map onechar (bythrees bs)
    where
        bythrees :: [Bit] -> [[Bit]]
        bythrees [] = []
        bythrees bitlist = a : bythrees b
            where
                (a,b) = splitAt 3 bitlist
        onechar (0:0:0:_) = "0  "
        onechar (0:0:1:_) = "1  "
        onechar (0:1:0:_) = "2  "
        onechar (0:1:1:_) = "3  "
        onechar (1:0:0:_) = "4  "
        onechar (1:0:1:_) = "5  "
        onechar (1:1:0:_) = "6  "
        onechar (1:1:1:_) = "7  "

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

printFieldList :: Show a => (Field a -> String) -> [Field a] -> String
printFieldList labeltostring fieldlist = concat $ map indent tostrings
    where
        zippedfields = zip (fieldbitoffsets fieldlist) fieldlist
        filtered = filter (\(_,f) -> shouldshowfield f) zippedfields
        tostrings = map (\(i,f) -> (i,fieldstring labeltostring f)) filtered
        indent (i,(bs,label)) = (replicate i ' ') ++ bs ++ (replicate (labelcolumn-i-(length bs)) ' ') ++ label ++ "\n"

printInstruction :: Show a => (Field a -> String) -> Instruction String a -> String
printInstruction labeltostring instr = (tooctal $ instructionbits instr) ++ "\n" ++ (instructionString instr) ++ (printFieldList labeltostring $ fields instr) ++ "\n"
    where
        fields (Instruction _ thefields) = thefields

-- vim:sw=4:ts=4:et:ai:
