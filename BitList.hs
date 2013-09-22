
--
-- BitList.hs
--

-- most significant bit listed first
-- not using the functions in Data.Vector.Unboxed.Bit (bitvec) since they
-- operate on Words and I want to operate on Word8s

{- "Note that the name of a type and its constructors have be grouped together,
as in Tree(Leaf,Branch). As short-hand, we could also write Tree(..). Exporting
a subset of the constructors is also possible. The names in an export list need
not be local to the exporting module; any name in scope may be listed in an
export list." -}

module BitList where

import qualified Data.ByteString as B
import Data.Word (Word8,Word16)
import Data.Bit  (Bit,fromBool)
import Data.Bits (testBit)

-- can I use read for this?
bitsFromString :: String -> [Bit]
bitsFromString s = map charToBit s
    where
        charToBit '0' = 0
        charToBit '1' = 1

-- is there already a function for this?
bitsFromByte :: Word8 -> [Bit]
bitsFromByte byte = [ fromBool $ testBit byte i | i <- [7,6..0] ]

bitsFromByteList :: [Word8] -> [Bit]
bitsFromByteList = concatMap bitsFromByte

bitsFromOctalWord :: Word16 -> [Bit]
bitsFromOctalWord word = [ fromBool $ testBit word i | i <- [11,10..0] ]

bitsFromOctalWordList :: [Word16] -> [Bit]
bitsFromOctalWordList = concatMap bitsFromOctalWord

-- is there already a function for this?
bitsFromByteString :: B.ByteString -> [Bit]
bitsFromByteString bs = case B.uncons bs of
    Nothing -> []
    Just (w, rest) -> bitsFromByte w ++ bitsFromByteString rest

bitstostring :: [Bit] -> String
bitstostring = concat . map show

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

-- vim:sw=4:ts=4:et:ai:
