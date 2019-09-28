{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
--------------------------------------------------------------------------------
module Xanthous.Game.State
  ( GameState(..)
  , entities
  , revealedPositions
  , messageHistory
  , randomGen
  , promptState
  , characterEntityID
  , GamePromptState(..)

    -- * Messages
  , MessageHistory(..)
  , pushMessage
  , popMessage
  , hideMessage

    -- * App monad
  , AppT(..)
  , AppM

    -- * Entities
  , Draw(..)
  , Brain(..)
  , Brainless(..)
  , brainVia
  , Entity(..)
  , SomeEntity(..)
  , downcastEntity
  , _SomeEntity
  , entityIs

    -- * Debug State
  , DebugState(..)
  , debugState
  , allRevealed
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Data.List.NonEmpty ( NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Typeable
import           Data.Coerce
import           System.Random
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic
import           Control.Monad.State.Class
import           Control.Monad.State
import           Control.Monad.Random.Class
import           Brick (EventM, Widget)
--------------------------------------------------------------------------------
import           Xanthous.Data.EntityMap (EntityMap, EntityID)
import           Xanthous.Data (Positioned(..), Position(..), Neighbors)
import           Xanthous.Orphans ()
import           Xanthous.Game.Prompt
import           Xanthous.Resource
--------------------------------------------------------------------------------

data MessageHistory
  = NoMessageHistory
  | MessageHistory (NonEmpty Text) Bool
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)

instance Arbitrary MessageHistory where
  arbitrary = genericArbitrary

pushMessage :: Text -> MessageHistory -> MessageHistory
pushMessage msg NoMessageHistory = MessageHistory (msg :| []) True
pushMessage msg (MessageHistory msgs _) = MessageHistory (NonEmpty.cons msg msgs) True

popMessage :: MessageHistory -> MessageHistory
popMessage NoMessageHistory = NoMessageHistory
popMessage (MessageHistory msgs False) = MessageHistory msgs True
popMessage (MessageHistory msgs@(_ :| []) _) = MessageHistory msgs True
popMessage (MessageHistory (_ :| (msg : msgs)) True) = MessageHistory (msg :| msgs) True

hideMessage :: MessageHistory -> MessageHistory
hideMessage NoMessageHistory = NoMessageHistory
hideMessage (MessageHistory msgs _) = MessageHistory msgs False

--------------------------------------------------------------------------------

data GamePromptState m where
  NoPrompt :: GamePromptState m
  WaitingPrompt :: Text -> Prompt m -> GamePromptState m
  deriving stock (Show)

--------------------------------------------------------------------------------

newtype AppT m a
  = AppT { unAppT :: StateT GameState m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState GameState
           )
       via (StateT GameState m)

type AppM = AppT (EventM Name)

--------------------------------------------------------------------------------

class Draw a where
  drawWithNeighbors :: Neighbors (Vector SomeEntity) -> a -> Widget n
  drawWithNeighbors = const draw

  draw :: a -> Widget n
  draw = drawWithNeighbors $ pure mempty

instance Draw a => Draw (Positioned a) where
  drawWithNeighbors ns (Positioned _ a) = drawWithNeighbors ns a
  draw (Positioned _ a) = draw a

--------------------------------------------------------------------------------

class Brain a where
  step :: Positioned a -> AppM (Positioned a)

newtype Brainless a = Brainless a

instance Brain (Brainless a) where
  step = pure

-- | Workaround for the inability to use DerivingVia on Brain due to the lack of
-- higher-order roles (specifically AppT not having its last type argument have
-- role representational bc of StateT)
brainVia
  :: forall brain entity. (Coercible entity brain, Brain brain)
  => (entity -> brain) -- ^ constructor, ignored
  -> (Positioned entity -> AppM (Positioned entity))
brainVia _ = fmap coerce . step . coerce @_ @(Positioned brain)

--------------------------------------------------------------------------------

class (Show a, Eq a, Draw a, Brain a) => Entity a where
  blocksVision :: a -> Bool
  description :: a -> Text

data SomeEntity where
  SomeEntity :: forall a. (Entity a, Typeable a) => a -> SomeEntity

instance Show SomeEntity where
  show (SomeEntity e) = "SomeEntity (" <> show e <> ")"

instance Eq SomeEntity where
  (SomeEntity (a :: ea)) == (SomeEntity (b :: eb)) = case eqT @ea @eb of
    Just Refl -> a == b
    _ -> False

instance Draw (SomeEntity) where
  drawWithNeighbors ns (SomeEntity ent) = drawWithNeighbors ns ent

instance Brain SomeEntity where
  step (Positioned pos (SomeEntity ent)) =
    fmap SomeEntity <$> step (Positioned pos ent)

instance Entity SomeEntity where
  blocksVision (SomeEntity ent) = blocksVision ent
  description (SomeEntity ent) = description ent

downcastEntity :: forall (a :: Type). (Typeable a) => SomeEntity -> Maybe a
downcastEntity (SomeEntity e) = cast e

entityIs :: forall (a :: Type). (Typeable a) => SomeEntity -> Bool
entityIs = isJust . downcastEntity @a

_SomeEntity :: forall a. (Entity a, Typeable a) => Prism' SomeEntity a
_SomeEntity = prism' SomeEntity downcastEntity

--------------------------------------------------------------------------------

data DebugState = DebugState
  { _allRevealed :: !Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)

instance Arbitrary DebugState where
  arbitrary = genericArbitrary

data GameState = GameState
  { _entities          :: !(EntityMap SomeEntity)
  , _revealedPositions :: !(Set Position)
  , _characterEntityID :: !EntityID
  , _messageHistory    :: !MessageHistory
  , _randomGen         :: !StdGen
  , _promptState       :: !(GamePromptState AppM)
  , _debugState        :: DebugState
  }
  deriving stock (Show)
makeLenses ''GameState

instance Eq GameState where
  (==) = (==) `on` \gs ->
    ( gs ^. entities
    , gs ^. revealedPositions
    , gs ^. characterEntityID
    , gs ^. messageHistory
    )

--------------------------------------------------------------------------------

instance MonadTrans AppT where
  lift = AppT . lift

instance (Monad m) => MonadRandom (AppT m) where
  getRandomR rng = randomGen %%= randomR rng
  getRandom = randomGen %%= random
  getRandomRs rng = uses randomGen $ randomRs rng
  getRandoms = uses randomGen randoms

--------------------------------------------------------------------------------

makeLenses ''DebugState
