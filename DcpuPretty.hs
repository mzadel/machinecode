
--
-- DcpuPretty.hs
--

module DcpuPretty where

import qualified DcpuSpecTables as Dcpu
import CodeAst
import BitList (bitsToWord16)
import Text.Printf

labeltostring :: Show a => Field (Dcpu.FieldType, a) -> String
labeltostring (FieldVariable (Dcpu.RegA,label) _) = "Register A: " ++ show label
labeltostring (FieldVariable (Dcpu.RegB,label) _) = "Register B: " ++ show label
labeltostring (FieldVariable (Dcpu.RegADataWord,_) bs) = "Register A data: " ++ (printf "0x%04x" value) ++ " == " ++ (show value)
    where value = bitsToWord16 bs
labeltostring (FieldVariable (Dcpu.RegBDataWord,_) bs) = "Register B data: " ++ (printf "0x%04x" value) ++ " == " ++ (show value)
    where value = bitsToWord16 bs

-- vim:sw=4:ts=4:et:ai:
