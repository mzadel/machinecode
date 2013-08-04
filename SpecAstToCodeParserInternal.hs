
--
-- SpecAstToCodeParserInternal.hs
--

{-# LANGUAGE FlexibleContexts #-}

module SpecAstToCodeParserInternal where

import Text.Parsec.Prim
import Text.Parsec.Pos
import Data.Bit

satisfy :: (Stream s m Bit) => (Bit -> Bool) -> ParsecT s u m Bit
satisfy f           = tokenPrim (\c -> show [c])
                                (\pos _ _cs -> updatePosChar pos ' ' )
                                (\c -> if f c then Just c else Nothing)

matchBit :: (Stream s m Bit) => Bit -> ParsecT s u m Bit
matchBit b = satisfy (==b) <?> show [b]

anyBit :: (Stream s m Bit) => ParsecT s u m Bit
anyBit             = satisfy (const True)

-- vim:sw=4:ts=4:et:ai:
