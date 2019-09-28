module Xanthous.Monad
  ( AppT(..)
  , AppM
  , runAppT
  , continue
  , halt
  , say
  , say_
  ) where

import Xanthous.Prelude
import Control.Monad.Random
import Control.Monad.State
import qualified Brick
import Brick (EventM, Next)
import Data.Aeson

import Xanthous.Game.State
import Xanthous.Messages (message)

runAppT :: Monad m => AppT m a -> GameState -> m (a, GameState)
runAppT appt initialState = flip runStateT initialState . unAppT $ appt

halt :: AppT (EventM n) (Next GameState)
halt = lift . Brick.halt =<< get

continue :: AppT (EventM n) (Next GameState)
continue = lift . Brick.continue =<< get


say :: (MonadRandom m, ToJSON params, MonadState GameState m)
    => [Text] -> params -> m ()
say msgPath params = do
  msg <- message msgPath params
  messageHistory %= pushMessage msg

say_ :: (MonadRandom m, MonadState GameState m) => [Text] -> m ()
say_ msgPath = say msgPath $ object []
