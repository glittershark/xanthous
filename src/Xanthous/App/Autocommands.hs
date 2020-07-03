--------------------------------------------------------------------------------
module Xanthous.App.Autocommands
  ( runAutocommand
  , autoStep
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import qualified Data.Aeson as A
import           Data.Aeson (object)
import           Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE
import           Control.Monad.State (gets)
--------------------------------------------------------------------------------
import           Xanthous.App.Common
import           Xanthous.App.Time
import           Xanthous.Data
import           Xanthous.Data.App
import           Xanthous.Entities.Character (speed)
import           Xanthous.Entities.Creature (Creature, creatureType)
import           Xanthous.Entities.RawTypes (hostile)
import           Xanthous.Game.State
--------------------------------------------------------------------------------

autoStep :: Autocommand -> AppM ()
autoStep (AutoMove dir) = do
  newPos <- uses characterPosition $ move dir
  collisionAt newPos >>= \case
    Nothing -> do
      characterPosition .= newPos
      stepGameBy =<< uses (character . speed) (|*| 1)
      describeEntitiesAt newPos
      maybeVisibleEnemies <- nonEmpty <$> enemiesInSight
      for_ maybeVisibleEnemies $ \visibleEnemies -> do
        say ["autoMove", "enemyInSight"]
          $ object [ "firstEntity" A..= NE.head visibleEnemies ]
        cancelAutocommand
    Just _ -> cancelAutocommand
  where
    enemiesInSight :: AppM [Creature]
    enemiesInSight = do
      ents <- gets characterVisibleEntities
      pure $ ents
         ^.. folded
           . _SomeEntity @Creature
           . filtered (view $ creatureType . hostile)

--------------------------------------------------------------------------------

autocommandIntervalμs :: Int
autocommandIntervalμs = 1000 * 50 -- 50 ms

runAutocommand :: Autocommand -> AppM ()
runAutocommand ac = do
  env <- ask
  tid <- liftIO . async $ runReaderT go env
  autocommand .= ActiveAutocommand ac tid
  where
    go = everyμs autocommandIntervalμs $ sendEvent AutoContinue

-- | Perform 'act' every μs microseconds forever
everyμs :: MonadIO m => Int -> m () -> m ()
everyμs μs act = act >> liftIO (threadDelay μs) >> everyμs μs act
