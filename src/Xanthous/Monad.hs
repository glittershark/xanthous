--------------------------------------------------------------------------------
module Xanthous.Monad
  ( AppT(..)
  , AppM
  , runAppT
  , continue
  , halt
    -- * Messages
  , say
  , say_
  , message
  , message_
  , writeMessage
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
import           Control.Monad.Random
import           Control.Monad.State
import qualified Brick
import           Brick (EventM, Next)
import           Data.Aeson
--------------------------------------------------------------------------------
import           Xanthous.Game.State
import           Xanthous.Messages (Message)
import qualified Xanthous.Messages as Messages
--------------------------------------------------------------------------------

runAppT :: Monad m => AppT m a -> GameState -> m (a, GameState)
runAppT appt initialState = flip runStateT initialState . unAppT $ appt

halt :: AppT (EventM n) (Next GameState)
halt = lift . Brick.halt =<< get

continue :: AppT (EventM n) (Next GameState)
continue = lift . Brick.continue =<< get

--------------------------------------------------------------------------------

say :: (MonadRandom m, ToJSON params, MonadState GameState m)
    => [Text] -> params -> m ()
say msgPath = writeMessage <=< Messages.message msgPath

say_ :: (MonadRandom m, MonadState GameState m) => [Text] -> m ()
say_ msgPath = say msgPath $ object []

message :: (MonadRandom m, ToJSON params, MonadState GameState m)
        => Message -> params -> m ()
message msg = writeMessage <=< Messages.render msg

message_ :: (MonadRandom m, MonadState GameState m)
         => Message ->  m ()
message_ msg = message msg $ object []

writeMessage :: MonadState GameState m => Text -> m ()
writeMessage m = messageHistory %= pushMessage m
