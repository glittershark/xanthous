{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
--------------------------------------------------------------------------------
module Xanthous.Game.State
  ( GameState(..)
  , entities
  , levels
  , revealedPositions
  , messageHistory
  , randomGen
  , activePanel
  , promptState
  , characterEntityID
  , autocommand
  , memo
  , GamePromptState(..)

    -- * Game Level
  , GameLevel(..)
  , levelEntities
  , upStaircasePosition
  , levelRevealedPositions

    -- * Messages
  , MessageHistory(..)
  , HasMessages(..)
  , HasTurn(..)
  , HasDisplayedTurn(..)
  , pushMessage
  , previousMessage
  , nextTurn

    -- * Autocommands
  , Autocommand(..)
  , AutocommandState(..)
  , _NoAutocommand
  , _ActiveAutocommand

    -- * App monad
  , AppT(..)
  , AppM
  , runAppT

    -- * Entities
  , Draw(..)
  , Brain(..)
  , Brainless(..)
  , brainVia
  , Collision(..)
  , Entity(..)
  , SomeEntity(..)
  , downcastEntity
  , _SomeEntity
  , entityIs
    -- ** Vias
  , Color(..)
  , DrawNothing(..)
  , DrawRawChar(..)
  , DrawRawCharPriority(..)
  , DrawCharacter(..)
  , DrawStyledCharacter(..)
  , DeriveEntity(..)
    -- ** Field classes
  , HasChar(..)
  , HasStyle(..)

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
import           Control.Monad.Random.Class
import           Control.Monad.State
import           Control.Monad.Trans.Control (MonadTransControl(..))
import           Control.Monad.Trans.Compose
import           Control.Monad.Morph (MFunctor(..))
import           Brick (EventM, Widget, raw, str, emptyWidget)
import           Data.Aeson (ToJSON(..), FromJSON(..), Value(Null))
import qualified Data.Aeson as JSON
import           Data.Aeson.Generic.DerivingVia
import           Data.Generics.Product.Fields
import qualified Graphics.Vty.Attributes as Vty
import qualified Graphics.Vty.Image as Vty
--------------------------------------------------------------------------------
import           Xanthous.Util (KnownBool(..))
import           Xanthous.Util.QuickCheck (GenericArbitrary(..))
import           Xanthous.Data
import           Xanthous.Data.App
import           Xanthous.Data.Levels
import           Xanthous.Data.EntityMap (EntityMap, EntityID)
import           Xanthous.Data.EntityChar
import           Xanthous.Data.VectorBag
import           Xanthous.Data.Entities
import           Xanthous.Orphans ()
import           Xanthous.Game.Prompt
import           Xanthous.Game.Env
import           Xanthous.Game.Memo (MemoState)
--------------------------------------------------------------------------------

data MessageHistory
  = MessageHistory
  { _messages      :: Map Word (NonEmpty Text)
  , _turn          :: Word
  , _displayedTurn :: Maybe Word
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary MessageHistory
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
  = AppT { unAppT :: ReaderT GameEnv (StateT GameState m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState GameState
           , MonadReader GameEnv
           , MonadIO
           )
       via (ReaderT GameEnv (StateT GameState m))
  deriving ( MonadTrans
           , MFunctor
           )
       via (ReaderT GameEnv `ComposeT` StateT GameState)

type AppM = AppT (EventM ResourceName)

--------------------------------------------------------------------------------

class Draw a where
  drawWithNeighbors :: Neighbors (VectorBag SomeEntity) -> a -> Widget n
  drawWithNeighbors = const draw

  draw :: a -> Widget n
  draw = drawWithNeighbors $ pure mempty

  -- | higher priority gets drawn on top
  drawPriority :: a -> Word
  drawPriority = const minBound

instance Draw a => Draw (Positioned a) where
  drawWithNeighbors ns (Positioned _ a) = drawWithNeighbors ns a
  draw (Positioned _ a) = draw a

newtype DrawCharacter (char :: Symbol) (a :: Type) where
  DrawCharacter :: a -> DrawCharacter char a

instance KnownSymbol char => Draw (DrawCharacter char a) where
  draw _ = str $ symbolVal @char Proxy

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

class KnownColor (color :: Color) where
  colorVal :: forall proxy. proxy color -> Vty.Color

instance KnownColor 'Black where colorVal _ = Vty.black
instance KnownColor 'Red where colorVal _ = Vty.red
instance KnownColor 'Green where colorVal _ = Vty.green
instance KnownColor 'Yellow where colorVal _ = Vty.yellow
instance KnownColor 'Blue where colorVal _ = Vty.blue
instance KnownColor 'Magenta where colorVal _ = Vty.magenta
instance KnownColor 'Cyan where colorVal _ = Vty.cyan
instance KnownColor 'White where colorVal _ = Vty.white

class KnownMaybeColor (maybeColor :: Maybe Color) where
  maybeColorVal :: forall proxy. proxy maybeColor -> Maybe Vty.Color

instance KnownMaybeColor 'Nothing where maybeColorVal _ = Nothing
instance KnownColor color => KnownMaybeColor ('Just color) where
  maybeColorVal _ = Just $ colorVal @color Proxy

newtype DrawStyledCharacter (fg :: Maybe Color) (bg :: Maybe Color) (char :: Symbol) (a :: Type) where
  DrawStyledCharacter :: a -> DrawStyledCharacter fg bg char a

instance
  ( KnownMaybeColor fg
  , KnownMaybeColor bg
  , KnownSymbol char
  )
  => Draw (DrawStyledCharacter fg bg char a) where
  draw _ = raw $ Vty.string attr $ symbolVal @char Proxy
    where attr = Vty.Attr
            { Vty.attrStyle = Vty.Default
            , Vty.attrForeColor = maybe Vty.Default Vty.SetTo
                                  $ maybeColorVal @fg Proxy
            , Vty.attrBackColor = maybe Vty.Default Vty.SetTo
                                  $ maybeColorVal @bg Proxy
            , Vty.attrURL = Vty.Default
            }

instance Draw EntityChar where
  draw EntityChar{..} = raw $ Vty.string _style [_char]

--------------------------------------------------------------------------------

newtype DrawNothing (a :: Type) = DrawNothing a

instance Draw (DrawNothing a) where
  draw = const emptyWidget
  drawPriority = const 0

newtype DrawRawChar (rawField :: Symbol) (a :: Type) = DrawRawChar a

instance
  forall rawField a raw.
  ( HasField rawField a a raw raw
  , HasChar raw EntityChar
  ) => Draw (DrawRawChar rawField a) where
  draw (DrawRawChar e) = draw $ e ^. field @rawField . char

newtype DrawRawCharPriority
  (rawField :: Symbol)
  (priority :: Nat)
  (a :: Type)
  = DrawRawCharPriority a

instance
  forall rawField priority a raw.
  ( HasField rawField a a raw raw
  , KnownNat priority
  , HasChar raw EntityChar
  ) => Draw (DrawRawCharPriority rawField priority a) where
  draw (DrawRawCharPriority e) = draw $ e ^. field @rawField . char
  drawPriority = const . fromIntegral $ natVal @priority Proxy


--------------------------------------------------------------------------------

class Brain a where
  step :: Ticks -> Positioned a -> AppM (Positioned a)
  -- | Does this entity ever move on its own?
  entityCanMove :: a -> Bool
  entityCanMove = const False

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

class ( Show a, Eq a, Ord a, NFData a
      , ToJSON a, FromJSON a
      , Draw a, Brain a
      ) => Entity a where
  entityAttributes :: a -> EntityAttributes
  entityAttributes = const defaultEntityAttributes
  description :: a -> Text
  entityChar :: a -> EntityChar
  entityCollision :: a -> Maybe Collision
  entityCollision = const $ Just Stop

data SomeEntity where
  SomeEntity :: forall a. (Entity a, Typeable a) => a -> SomeEntity

instance Show SomeEntity where
  show (SomeEntity e) = "SomeEntity (" <> show e <> ")"

instance Eq SomeEntity where
  (SomeEntity (a :: ea)) == (SomeEntity (b :: eb)) = case eqT @ea @eb of
    Just Refl -> a == b
    _ -> False

instance Ord SomeEntity where
  compare (SomeEntity (a :: ea)) (SomeEntity (b :: eb)) = case eqT @ea @eb of
    Just Refl -> compare a b
    _ -> compare (typeRep $ Proxy @ea) (typeRep $ Proxy @eb)


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
  step ticks (Positioned p (SomeEntity ent)) =
    fmap SomeEntity <$> step ticks (Positioned p ent)
  entityCanMove (SomeEntity ent) = entityCanMove ent

downcastEntity :: forall (a :: Type). (Typeable a) => SomeEntity -> Maybe a
downcastEntity (SomeEntity e) = cast e

entityIs :: forall (a :: Type). (Typeable a) => SomeEntity -> Bool
entityIs = isJust . downcastEntity @a

_SomeEntity :: forall a. (Entity a, Typeable a) => Prism' SomeEntity a
_SomeEntity = prism' SomeEntity downcastEntity

newtype DeriveEntity
  (blocksVision :: Bool)
  (description :: Symbol)
  (entityChar :: Symbol)
  (entity :: Type)
  = DeriveEntity entity
  deriving newtype (Show, Eq, Ord, NFData, ToJSON, FromJSON, Draw)

instance Brain entity => Brain (DeriveEntity b d c entity) where
  step = brainVia $ \(DeriveEntity e) -> e

instance
  ( KnownBool blocksVision
  , KnownSymbol description
  , KnownSymbol entityChar
  , Show entity, Eq entity, Ord entity, NFData entity
  , ToJSON entity, FromJSON entity
  , Draw entity, Brain entity
  )
  => Entity (DeriveEntity blocksVision description entityChar entity) where
  entityAttributes _ = defaultEntityAttributes
    & blocksVision .~ boolVal @blocksVision
  description _ = pack . symbolVal $ Proxy @description
  entityChar _ = fromString . symbolVal $ Proxy @entityChar

--------------------------------------------------------------------------------

data GameLevel = GameLevel
  { _levelEntities :: !(EntityMap SomeEntity)
  , _upStaircasePosition :: !Position
  , _levelRevealedPositions :: !(Set Position)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
  deriving (ToJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
           GameLevel

--------------------------------------------------------------------------------

data Autocommand
  = AutoMove Direction
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable, ToJSON, FromJSON, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary Autocommand
{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}

data AutocommandState
  = NoAutocommand
  | ActiveAutocommand Autocommand (Async ())
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable)

instance Show AutocommandState where
  show NoAutocommand = "NoAutocommand"
  show (ActiveAutocommand ac _) =
    "(ActiveAutocommand " <> show ac <> " <Async>)"

instance ToJSON AutocommandState where
  toJSON = const Null

instance FromJSON AutocommandState where
  parseJSON Null = pure NoAutocommand
  parseJSON _ = fail "Invalid AutocommandState; expected null"

instance NFData AutocommandState where
  rnf NoAutocommand = ()
  rnf (ActiveAutocommand ac t) = ac `deepseq` t `seq` ()

instance CoArbitrary AutocommandState where
  coarbitrary NoAutocommand = variant @Int 1
  coarbitrary (ActiveAutocommand ac t)
    = variant @Int 2
    . coarbitrary ac
    . coarbitrary (hash t)

instance Function AutocommandState where
  function = functionMap onlyNoAC (const NoAutocommand)
    where
      onlyNoAC NoAutocommand = ()
      onlyNoAC _ = error "Can't handle autocommands in Function"

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
  { _levels            :: !(Levels GameLevel)
  , _characterEntityID :: !EntityID
  , _messageHistory    :: !MessageHistory
  , _randomGen         :: !StdGen

    -- | The active panel displayed in the UI, if any
  , _activePanel       :: !(Maybe Panel)

  , _promptState       :: !(GamePromptState AppM)
  , _debugState        :: !DebugState
  , _autocommand       :: !AutocommandState

  , _memo              :: MemoState
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)
  deriving (ToJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
           GameState

makeLenses ''GameLevel
makeLenses ''GameState

entities :: Lens' GameState (EntityMap SomeEntity)
entities = levels . current . levelEntities

revealedPositions :: Lens' GameState (Set Position)
revealedPositions = levels . current . levelRevealedPositions

instance Eq GameState where
  (==) = (==) `on` \gs ->
    ( gs ^. entities
    , gs ^. revealedPositions
    , gs ^. characterEntityID
    , gs ^. messageHistory
    , gs ^. activePanel
    , gs ^. debugState
    )

--------------------------------------------------------------------------------

runAppT :: Monad m => AppT m a -> GameEnv -> GameState -> m (a, GameState)
runAppT appt env initialState
  = flip runStateT initialState
  . flip runReaderT env
  . unAppT
  $ appt

instance (Monad m) => MonadRandom (AppT m) where
  getRandomR rng = randomGen %%= randomR rng
  getRandom = randomGen %%= random
  getRandomRs rng = uses randomGen $ randomRs rng
  getRandoms = uses randomGen randoms

instance MonadTransControl AppT where
  type StT AppT a = (a, GameState)
  liftWith f
    = AppT
    . ReaderT $ \e
    -> StateT $ \s
    -> (,s) <$> f (\action -> runAppT action e s)
  restoreT = AppT . ReaderT . const . StateT . const

--------------------------------------------------------------------------------

makeLenses ''DebugState
makePrisms ''AutocommandState
