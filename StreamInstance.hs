
--
-- StreamInstance.hs
--

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module StreamInstance where

import Text.Parsec.Prim

-- instance of Stream taken from Text.Parsec.String.  I need it in multiple
-- places but I don't need the rest of what's in Text.Parsec.String, so I'm
-- just isolating it here.
instance (Monad m) => Stream [tok] m tok where
    uncons []     = return $ Nothing
    uncons (t:ts) = return $ Just (t,ts)
    {-# INLINE uncons #-}

-- vim:sw=4:ts=4:et:ai:
