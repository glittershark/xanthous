--------------------------------------------------------------------------------
module Xanthous.Util.Comonad
  ( -- * Store comonad utils
    replace
  , current
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import Control.Comonad.Store.Class
--------------------------------------------------------------------------------

-- | Replace the current position of a store comonad with a new value by
-- comparing positions
replace :: (Eq i, ComonadStore i w) => w a -> a -> w a
replace w x = w =>> \w' -> if pos w' == pos w then x else extract w'
{-# INLINE replace #-}

-- | Lens into the current position of a store comonad.
--
--     current = lens extract replace
current :: (Eq i, ComonadStore i w) => Lens' (w a) a
current = lens extract replace
{-# INLINE current #-}
