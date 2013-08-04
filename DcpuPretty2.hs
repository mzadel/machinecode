
--
-- DcpuPretty2.hs
--

module DcpPretty2 where

import qualified DcpuSpecTables as Dcpu
import CodeAst
import DcpuPretty

labeltostring :: Show a => Field (Dcpu.FieldType, a) -> String
labeltostring (FieldLiteral _) = ""
labeltostring (FieldNothing) = ""
labeltostring (FieldVariable (Dcpu.RegA,label) bs) = "Register A: " ++ show label
labeltostring (FieldVariable (Dcpu.RegB,label) bs) = "Register B: " ++ show label
labeltostring (FieldVariable (Dcpu.RegADataWord,label) bs) = "Register A data: " ++ bitstostring bs ++ " (" ++ (show $ bitsToWord16 bs) ++ ")"
labeltostring (FieldVariable (Dcpu.RegBDataWord,label) bs) = "Register B data: " ++ bitstostring bs ++ " (" ++ (show $ bitsToWord16 bs) ++ ")"

-- vim:sw=4:ts=4:et:ai:
