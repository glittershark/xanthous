{-# LANGUAGE UndecidableInstances #-}
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
  , HasMessages(..)
  , HasTurn(..)
  , HasDisplayedTurn(..)
  , pushMessage
  , previousMessage
  , nextTurn

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
import           Data.Aeson (ToJSON(..), FromJSON(..), Value(Null))
import qualified Data.Aeson as JSON
import           Data.Aeson.Generic.DerivingVia
--------------------------------------------------------------------------------
import           Xanthous.Data.EntityMap (EntityMap, EntityID)
import           Xanthous.Data
import           Xanthous.Orphans ()
import           Xanthous.Game.Prompt
import           Xanthous.Resource
--------------------------------------------------------------------------------

data MessageHistory
  = MessageHistory
  { _messages      :: Map Word (NonEmpty Text)
  , _turn          :: Word
  , _displayedTurn :: Maybe Word
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
           MessageHistory
makeFieldsNoPrefix ''MessageHistory

instance Semigroup MessageHistory where
  (MessageHistory msgs₁ turn₁ dt₁) <> (MessageHistory msgs₂ turn₂ dt₂) =
    MessageHistory (msgs₁ <> msgs₂) (max turn₁ turn₂) $ case (dt₁, dt₂) of
      (_, Nothing)      -> Nothing
      (Just t, _)       -> Just t
      (Nothing, Just t) -> Just t

instance Monoid MessageHistory where
  mempty = MessageHistory mempty 0 Nothing

instance Arbitrary MessageHistory where
  arbitrary = genericArbitrary

type instance Element MessageHistory = [Text]
instance MonoFunctor MessageHistory where
  omap f mh@(MessageHistory _ t _) =
    mh & messages . at t %~ (NonEmpty.nonEmpty . f . toList =<<)

instance MonoComonad MessageHistory where
  oextract (MessageHistory ms t dt) = maybe [] toList $ ms ^. at (fromMaybe t dt)
  oextend cok mh@(MessageHistory _ t dt) =
    mh & messages . at (fromMaybe t dt) .~ NonEmpty.nonEmpty (cok mh)

pushMessage :: Text -> MessageHistory -> MessageHistory
pushMessage msg mh@(MessageHistory _ turn' _) =
  mh
  & messages . at turn' %~ \case
    Nothing -> Just $ msg :| mempty
    Just msgs -> Just $ msg <| msgs
  & displayedTurn .~ Nothing

nextTurn :: MessageHistory -> MessageHistory
nextTurn = (turn +~ 1) . (displayedTurn .~ Nothing)

previousMessage :: MessageHistory -> MessageHistory
previousMessage mh = mh & displayedTurn .~ maximumOf
  (messages . ifolded . asIndex . filtered (< mh ^. turn))
  mh


--------------------------------------------------------------------------------

data GamePromptState m where
  NoPrompt :: GamePromptState m
  WaitingPrompt :: Text -> Prompt m -> GamePromptState m
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Non-injective! We never try to serialize waiting prompts, since:
--
--  * they contain callback functions
--  * we can't save the game when in a prompt anyway
instance ToJSON (GamePromptState m) where
  toJSON _ = Null

-- | Always expects Null
instance FromJSON (GamePromptState m) where
  parseJSON Null = pure NoPrompt
  parseJSON _ = fail "Invalid GamePromptState; expected null"

instance CoArbitrary (GamePromptState m) where
  coarbitrary NoPrompt = variant @Int 1
  coarbitrary (WaitingPrompt txt _) = variant @Int 2 . coarbitrary txt

instance Function (GamePromptState m) where
  function = functionMap onlyNoPrompt (const NoPrompt)
    where
      onlyNoPrompt NoPrompt = ()
      onlyNoPrompt (WaitingPrompt _ _) =
        error "Can't handle prompts in Function!"

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

  -- | higher priority gets drawn on top
  drawPriority :: a -> Word
  drawPriority = const minBound

instance Draw a => Draw (Positioned a) where
  drawWithNeighbors ns (Positioned _ a) = drawWithNeighbors ns a
  draw (Positioned _ a) = draw a

--------------------------------------------------------------------------------

class Brain a where
  step :: Ticks -> Positioned a -> AppM (Positioned a)

newtype Brainless a = Brainless a

instance Brain (Brainless a) where
  step = const pure

-- | Workaround for the inability to use DerivingVia on Brain due to the lack of
-- higher-order roles (specifically AppT not having its last type argument have
-- role representational bc of StateT)
brainVia
  :: forall brain entity. (Coercible entity brain, Brain brain)
  => (entity -> brain) -- ^ constructor, ignored
  -> (Ticks -> Positioned entity -> AppM (Positioned entity))
brainVia _ ticks = fmap coerce . step ticks . coerce @_ @(Positioned brain)

--------------------------------------------------------------------------------

class ( Show a, Eq a, NFData a
      , ToJSON a, FromJSON a
      , Draw a, Brain a
      ) => Entity a where
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

instance NFData SomeEntity where
  rnf (SomeEntity ent) = ent `deepseq` ()

instance ToJSON SomeEntity where
  toJSON (SomeEntity ent) = entityToJSON ent
    where
      entityToJSON :: forall entity. (Entity entity, Typeable entity)
                   => entity -> JSON.Value
      entityToJSON entity = JSON.object
        [ "type" JSON..= tshow (typeRep @_ @entity Proxy)
        , "data" JSON..= toJSON entity
        ]

instance Draw SomeEntity where
  drawWithNeighbors ns (SomeEntity ent) = drawWithNeighbors ns ent
  drawPriority (SomeEntity ent) = drawPriority ent

instance Brain SomeEntity where
  step ticks (Positioned pos (SomeEntity ent)) =
    fmap SomeEntity <$> step ticks (Positioned pos ent)

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
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
           DebugState
{-# ANN DebugState ("HLint: ignore Use newtype instead of data" :: String) #-}

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
  deriving stock (Show, Generic)
  deriving anyclass (NFData)
  deriving (ToJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
           GameState
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

instance (MonadIO m) => MonadIO (AppT m) where
  liftIO = lift . liftIO

--------------------------------------------------------------------------------

makeLenses ''DebugState

--------------------------------------------------------------------------------

-- saveGame :: GameState -> LByteString
-- saveGame = Zlib.compress . JSON.encode

-- loadGame :: LByteString -> Maybe GameState
-- loadGame = JSON.decode . Zlib.decompress

-- saved :: Prism' LByteString GameState
-- saved = prism' saveGame loadGame
