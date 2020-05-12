--------------------------------------------------------------------------------
module Xanthous.App.Common
  ( describeEntities
  , describeEntitiesAt
  , entitiesAtPositionWithType

    -- * Re-exports
  , MonadState
  , MonadRandom
  , EntityMap
  , module Xanthous.Game.Lenses
  , module Xanthous.Monad
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Data.Aeson (object)
import qualified Data.Aeson as A
import           Control.Monad.State (MonadState)
import           Control.Monad.Random (MonadRandom)
--------------------------------------------------------------------------------
import           Xanthous.Data (Position, positioned)
import           Xanthous.Data.EntityMap (EntityMap)
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Game
import           Xanthous.Game.Lenses
import           Xanthous.Game.State
import           Xanthous.Monad
import           Xanthous.Entities.Character (Character)
import           Xanthous.Util.Inflection (toSentence)
--------------------------------------------------------------------------------

entitiesAtPositionWithType
  :: forall a. (Entity a, Typeable a)
  => Position
  -> EntityMap SomeEntity
  -> [(EntityMap.EntityID, a)]
entitiesAtPositionWithType pos em =
  let someEnts = EntityMap.atPositionWithIDs pos em
  in flip foldMap someEnts $ \(eid, view positioned -> se) ->
    case downcastEntity @a se of
      Just e  -> [(eid, e)]
      Nothing -> []

describeEntitiesAt :: (MonadState GameState m, MonadRandom m) => Position -> m ()
describeEntitiesAt pos =
  use ( entities
      . EntityMap.atPosition pos
      . to (filter (not . entityIs @Character))
      ) >>= \case
        Empty -> pure ()
        ents  -> describeEntities ents

describeEntities
  :: ( Entity entity
    , MonadRandom m
    , MonadState GameState m
    , MonoFoldable (f Text)
    , Functor f
    , Element (f Text) ~ Text
    )
  => f entity
  -> m ()
describeEntities ents =
  let descriptions = description <$> ents
  in say ["entities", "description"]
     $ object ["entityDescriptions" A..= toSentence descriptions]
