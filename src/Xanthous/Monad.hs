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

    -- * Autocommands
  , cancelAutocommand

    -- * Events
  , sendEvent
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
import           Control.Monad.Random
import           Control.Monad.State
import qualified Brick
import           Brick (EventM, Next)
import           Brick.BChan (writeBChan)
import           Data.Aeson (ToJSON, object)
--------------------------------------------------------------------------------
import           Xanthous.Data.App (AppEvent)
import           Xanthous.Game.State
import           Xanthous.Game.Env
import           Xanthous.Messages (Message)
import qualified Xanthous.Messages as Messages
--------------------------------------------------------------------------------

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

-- | Cancel the currently active autocommand, if any
cancelAutocommand :: (MonadState GameState m, MonadIO m) => m ()
cancelAutocommand = do
  traverse_ (liftIO . cancel . snd) =<< preuse (autocommand . _ActiveAutocommand)
  autocommand .= NoAutocommand

--------------------------------------------------------------------------------

-- | Send an event to the app in an environment where the game env is available
sendEvent :: (MonadReader GameEnv m, MonadIO m) => AppEvent -> m ()
sendEvent evt = do
  ec <- view eventChan
  liftIO $ writeBChan ec evt
