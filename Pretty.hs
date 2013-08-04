
--
-- Pretty.hs
--

module Pretty where

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



-- vim:sw=4:ts=4:et:ai:
