
--
-- DcpuPretty.hs
--

module DcpuPretty (instructionString) where

import CodeAst
import Data.Bit

-- given the code ast for one instruction, return a string that pretty prints it

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
instructionString :: Instruction String b -> String
instructionString instr = (bitstostring $ instructionbits instr) ++ " " ++ (label instr) ++ "\n"
    where
        label (Instruction labelstring _) = labelstring

-- vim:sw=4:ts=4:et:ai:
