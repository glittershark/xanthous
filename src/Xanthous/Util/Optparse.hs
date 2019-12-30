--------------------------------------------------------------------------------
module Xanthous.Util.Optparse
  ( readWithGuard
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import qualified Options.Applicative as Opt
--------------------------------------------------------------------------------

readWithGuard
  :: Read b
  => (b -> Bool)
  -> (b -> String)
  -> Opt.ReadM b
readWithGuard predicate errmsg = do
  res <- Opt.auto
  unless (predicate res)
    $ Opt.readerError
    $ errmsg res
  pure res
