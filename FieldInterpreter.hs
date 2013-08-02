
--
-- FieldInterpreter.hs
--

module FieldInterpreter where

import Data.Bit

-- take a spec string and the bits found in them, and return an interpretation
-- based on the lookup table
fieldinterpreter :: [( String, [Bit], a )] -> String -> [Bit] -> a
fieldinterpreter table specstring parsedbits = head matchinglabels
    where
        (strings,bits,labels) = unzip3 table
        matchinglabels = [ l | s <- strings, b <- bits, l <- labels, s == specstring, b == parsedbits ]

-- vim:sw=4:ts=4:et:ai:
