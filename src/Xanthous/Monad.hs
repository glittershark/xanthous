module Xanthous.Monad
  ( AppT(..)
  , runAppT
  , continue
  , halt
  , say
  ) where

import Xanthous.Prelude
import Control.Monad.Random
import Control.Monad.State
import qualified Brick
import Brick (EventM, Next)
import Data.Aeson

import Xanthous.Game
import Xanthous.Messages (message)

newtype AppT m a
  = AppT { unAppT :: StateT GameState m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState GameState
           )
       via (StateT GameState m)

instance MonadTrans AppT where
  lift = AppT . lift

instance (Monad m) => MonadRandom (AppT m) where
  getRandomR rng = randomGen %%= randomR rng
  getRandom = randomGen %%= random
  getRandomRs rng = uses randomGen $ randomRs rng
  getRandoms = uses randomGen randoms

runAppT :: Monad m => AppT m a -> GameState -> m (a, GameState)
runAppT appt initialState = flip runStateT initialState . unAppT $ appt

halt :: AppT (EventM n) (Next GameState)
halt = lift . Brick.halt =<< get

continue :: AppT (EventM n) (Next GameState)
continue = lift . Brick.continue =<< get

-- say :: [Text] -> AppT m ()
-- say :: [Text] -> params -> AppT m ()

class SayR a where
  say :: [Text] -> a

instance Monad m => SayR (AppT m ()) where
  say msgPath = say msgPath $ object []

instance (Monad m, ToJSON params) => SayR (params -> AppT m ()) where
  say msgPath params = do
    msg <- message msgPath params
    messageHistory %= pushMessage msg
