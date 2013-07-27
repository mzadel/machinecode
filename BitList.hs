
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
import Data.Word (Word8)
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

-- is there already a function for this?
bitsFromByteString :: B.ByteString -> [Bit]
bitsFromByteString bs = case B.uncons bs of
    Nothing -> []
    Just (w, rest) -> bitsFromByte w ++ bitsFromByteString rest

-- vim:sw=4:ts=4:et:ai:
