{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
module Xanthous.App (makeApp) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
import           Brick hiding (App, halt, continue, raw)
import qualified Brick
import           Brick.Widgets.Edit (handleEditorEvent)
import           Graphics.Vty.Attributes (defAttr)
import           Graphics.Vty.Input.Events (Event(EvKey), Key(..))
import           Control.Monad.State (get, gets, MonadState)
import           Control.Monad.Random (MonadRandom)
import           Control.Monad.State.Class (modify)
import           Data.Aeson (object, ToJSON)
import qualified Data.Aeson as A
import qualified Data.Vector as V
import           System.Exit
import           GHC.TypeLits (TypeError, ErrorMessage(..))
--------------------------------------------------------------------------------
import           Xanthous.Command
import           Xanthous.Data
                 ( move
                 , Dimensions'(Dimensions)
                 , positioned
                 , Position
                 , Ticks
                 , (|*|)
                 )
import           Xanthous.Data.EntityMap (EntityMap)
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Game
import           Xanthous.Game.State
import           Xanthous.Game.Draw (drawGame)
import           Xanthous.Game.Prompt
import           Xanthous.Monad
import           Xanthous.Resource (Name)
import qualified Xanthous.Messages as Messages
import           Xanthous.Util.Inflection (toSentence)
--------------------------------------------------------------------------------
import qualified Xanthous.Entities.Character as Character
import           Xanthous.Entities.Character hiding (pickUpItem)
import           Xanthous.Entities.Item (Item)
import qualified Xanthous.Entities.Item as Item
import           Xanthous.Entities.Creature (Creature)
import qualified Xanthous.Entities.Creature as Creature
import           Xanthous.Entities.Environment
                 (Door, open, locked, GroundMessage(..))
import           Xanthous.Entities.RawTypes (edible, eatMessage, hitpointsHealed)
import           Xanthous.Generators
import qualified Xanthous.Generators.CaveAutomata as CaveAutomata
--------------------------------------------------------------------------------

type App = Brick.App GameState () Name

makeApp :: IO App
makeApp = pure $ Brick.App
  { appDraw = drawGame
  , appChooseCursor = const headMay
  , appHandleEvent = \game event -> runAppM (handleEvent event) game
  , appStartEvent = runAppM $ startEvent >> get
  , appAttrMap = const $ attrMap defAttr []
  }

runAppM :: AppM a -> GameState -> EventM Name a
runAppM appm = fmap fst . runAppT appm

startEvent :: AppM ()
startEvent = do
  initLevel
  modify updateCharacterVision
  use (character . characterName) >>= \case
    Nothing -> prompt_ @'StringPrompt ["character", "namePrompt"] Uncancellable
      $ \(StringResult s) -> do
        character . characterName ?= s
        say ["welcome"] =<< use character
    Just n -> say ["welcome"] $ object [ "characterName" A..= n ]

initLevel :: AppM ()
initLevel = do
  level <-
    generateLevel SCaveAutomata CaveAutomata.defaultParams
    $ Dimensions 80 80

  entities <>= (SomeEntity <$> level ^. levelWalls)
  entities <>= (SomeEntity <$> level ^. levelItems)
  entities <>= (SomeEntity <$> level ^. levelCreatures)
  entities <>= (SomeEntity <$> level ^. levelTutorialMessage)

  characterPosition .= level ^. levelCharacterPosition

--------------------------------------------------------------------------------

stepGameBy :: Ticks -> AppM ()
stepGameBy ticks = do
  ents <- uses entities EntityMap.toEIDsAndPositioned
  for_ ents $ \(eid, pEntity) -> do
    pEntity' <- step ticks pEntity
    entities . ix eid .= pEntity'

  whenM (uses character isDead)
    . prompt_ @'Continue ["dead"] Uncancellable
    . const . lift . liftIO
    $ exitSuccess

ticksPerTurn :: Ticks
ticksPerTurn = 100

stepGame :: AppM ()
stepGame = stepGameBy ticksPerTurn

--------------------------------------------------------------------------------

handleEvent :: BrickEvent Name () -> AppM (Next GameState)
handleEvent ev = use promptState >>= \case
  NoPrompt -> handleNoPromptEvent ev
  WaitingPrompt msg pr -> handlePromptEvent msg pr ev


handleNoPromptEvent :: BrickEvent Name () -> AppM (Next GameState)
handleNoPromptEvent (VtyEvent (EvKey k mods))
  | Just command <- commandFromKey k mods
  = do messageHistory %= nextTurn
       handleCommand command
handleNoPromptEvent _ = continue

handleCommand :: Command -> AppM (Next GameState)
handleCommand Quit = halt
handleCommand (Move dir) = do
  newPos <- uses characterPosition $ move dir
  collisionAt newPos >>= \case
    Nothing -> do
      characterPosition .= newPos
      stepGameBy =<< uses (character . speed) (|*| 1)
      describeEntitiesAt newPos
      modify updateCharacterVision
    Just Combat -> attackAt newPos
    Just Stop -> pure ()
  continue

handleCommand PickUp = do
  pos <- use characterPosition
  uses entities (entitiesAtPositionWithType @Item pos) >>= \case
    [] -> say_ ["pickUp", "nothingToPickUp"]
    [item] -> pickUpItem item
    items ->
      menu_ ["pickUp", "menu"] Cancellable (entityMenu_ items)
      $ \(MenuResult item) -> pickUpItem item
  continue
  where
    pickUpItem (itemID, item) = do
      character %= Character.pickUpItem item
      entities . at itemID .= Nothing
      say ["pickUp", "pickUp"] $ object [ "item" A..= item ]
      stepGameBy 100 -- TODO

handleCommand PreviousMessage = do
  messageHistory %= previousMessage
  continue

handleCommand Open = do
  prompt_ @'DirectionPrompt ["open", "prompt"] Cancellable
    $ \(DirectionResult dir) -> do
      pos <- move dir <$> use characterPosition
      doors <- uses entities $ entitiesAtPositionWithType @Door pos
      if | null doors -> say_ ["open", "nothingToOpen"]
         | any (view $ _2 . locked) doors -> say_ ["open", "locked"]
         | otherwise -> do
             for_ doors $ \(eid, _) ->
               entities . ix eid . positioned . _SomeEntity . open .= True
             say_ ["open", "success"]
      pure ()
  stepGame -- TODO
  continue

handleCommand Look = do
  prompt_ @'PointOnMap ["look", "prompt"] Cancellable
    $ \(PointOnMapResult pos) ->
      use (entities . EntityMap.atPosition pos)
      >>= \case
        Empty -> say_ ["look", "nothing"]
        ents -> describeEntities ents
  continue

handleCommand Wait = stepGame >> continue

handleCommand Eat = do
  uses (character . inventory)
       (V.mapMaybe (\item -> (item,) <$> item ^. Item.itemType . edible))
    >>= \case
      Empty -> say_ ["eat", "noFood"]
      food ->
        let foodMenuItem idx (item, edibleItem)
              = ( item ^. Item.itemType . char . char
                , MenuOption (description item) (idx, item, edibleItem))
                -- TODO refactor to use entityMenu_
            menuItems = mkMenuItems $ imap foodMenuItem food
        in menu_ ["eat", "menuPrompt"] Cancellable menuItems
          $ \(MenuResult (idx, item, edibleItem)) -> do
            character . inventory %= \inv ->
              let (before, after) = V.splitAt idx inv
              in before <> fromMaybe Empty (tailMay after)
            let msg = fromMaybe (Messages.lookup ["eat", "eat"])
                      $ edibleItem ^. eatMessage
            character . characterHitpoints' +=
              edibleItem ^. hitpointsHealed . to fromIntegral
            message msg $ object ["item" A..= item]
  stepGame -- TODO
  continue

handleCommand Read = do
  -- TODO allow reading things in the inventory (combo direction+menu prompt?)
  prompt_ @'DirectionPrompt ["read", "prompt"] Cancellable
    $ \(DirectionResult dir) -> do
      pos <- uses characterPosition $ move dir
      uses entities
        (fmap snd . entitiesAtPositionWithType @GroundMessage pos) >>= \case
          Empty -> say_ ["read", "nothing"]
          GroundMessage msg :< Empty ->
            say ["read", "result"] $ object ["message" A..= msg]
          msgs ->
            let readAndContinue Empty = pure ()
                readAndContinue (msg :< msgs') =
                  prompt @'Continue
                    ["read", "result"]
                    (object ["message" A..= msg])
                    Cancellable
                  . const
                  $ readAndContinue msgs'
                readAndContinue _ = error "this is total"
            in readAndContinue msgs
  continue

handleCommand Save = do
  -- TODO default save locations / config file?
  prompt_ @'StringPrompt ["save", "location"] Cancellable
    $ \(StringResult filename) -> do
      src <- gets saveGame
      lift . liftIO $ do
        writeFile (unpack filename) $ toStrict src
        exitSuccess

  continue


handleCommand ToggleRevealAll = do
  val <- debugState . allRevealed <%= not
  say ["debug", "toggleRevealAll"] $ object [ "revealAll" A..= val ]
  continue

--------------------------------------------------------------------------------

handlePromptEvent
  :: Text -- ^ Prompt message
  -> Prompt AppM
  -> BrickEvent Name ()
  -> AppM (Next GameState)

handlePromptEvent _ (Prompt Cancellable _ _ _ _) (VtyEvent (EvKey KEsc []))
  = clearPrompt
handlePromptEvent _ pr (VtyEvent (EvKey KEnter []))
  = submitPrompt pr >> clearPrompt

handlePromptEvent
  msg
  (Prompt c SStringPrompt (StringPromptState edit) pri cb)
  (VtyEvent ev)
  = do
    edit' <- lift $ handleEditorEvent ev edit
    let prompt' = Prompt c SStringPrompt (StringPromptState edit') pri cb
    promptState .= WaitingPrompt msg prompt'
    continue

handlePromptEvent _ (Prompt _ SDirectionPrompt _ _ cb)
  (VtyEvent (EvKey (KChar (directionFromChar -> Just dir)) []))
  = cb (DirectionResult dir) >> clearPrompt
handlePromptEvent _ (Prompt _ SDirectionPrompt _ _ _) _ = continue

handlePromptEvent _ (Prompt _ SContinue _ _ _) _ = continue

handlePromptEvent _ (Prompt _ SMenu _ items cb) (VtyEvent (EvKey (KChar chr) []))
  | Just (MenuOption _ res) <- items ^. at chr
  = cb (MenuResult res) >> clearPrompt
  | otherwise
  = continue

handlePromptEvent
  msg
  (Prompt c SPointOnMap (PointOnMapPromptState pos) pri cb)
  (VtyEvent (EvKey (KChar (directionFromChar -> Just dir)) []))
  = let pos' = move dir pos
        prompt' = Prompt c SPointOnMap (PointOnMapPromptState pos') pri cb
    in promptState .= WaitingPrompt msg prompt'
       >> continue
handlePromptEvent _ (Prompt _ SPointOnMap _ _ _) _ = continue

handlePromptEvent _ _ _ = continue

clearPrompt :: AppM (Next GameState)
clearPrompt = promptState .= NoPrompt >> continue

class NotMenu (pt :: PromptType)
instance NotMenu 'StringPrompt
instance NotMenu 'Confirm
instance NotMenu 'DirectionPrompt
instance NotMenu 'PointOnMap
instance NotMenu 'Continue
instance TypeError ('Text "Cannot use `prompt` or `prompt_` for menu prompts"
                    ':$$: 'Text "Use `menu` or `menu_` instead")
         => NotMenu ('Menu a)

prompt
  :: forall (pt :: PromptType) (params :: Type).
    (ToJSON params, SingPromptType pt, NotMenu pt)
  => [Text]                     -- ^ Message key
  -> params                     -- ^ Message params
  -> PromptCancellable
  -> (PromptResult pt -> AppM ()) -- ^ Prompt promise handler
  -> AppM ()
prompt msgPath params cancellable cb = do
  let pt = singPromptType @pt
  msg <- Messages.message msgPath params
  p <- case pt of
    SPointOnMap -> do
      charPos <- use characterPosition
      pure $ mkPointOnMapPrompt cancellable charPos cb
    SStringPrompt -> pure $ mkPrompt cancellable pt cb
    SConfirm -> pure $ mkPrompt cancellable pt cb
    SDirectionPrompt -> pure $ mkPrompt cancellable pt cb
    SContinue -> pure $ mkPrompt cancellable pt cb
    SMenu -> error "unreachable"
  promptState .= WaitingPrompt msg p

prompt_
  :: forall (pt :: PromptType).
    (SingPromptType pt, NotMenu pt)
  => [Text] -- ^ Message key
  -> PromptCancellable
  -> (PromptResult pt -> AppM ()) -- ^ Prompt promise handler
  -> AppM ()
prompt_ msg = prompt msg $ object []

menu :: forall (a :: Type) (params :: Type).
       (ToJSON params)
     => [Text]                            -- ^ Message key
     -> params                            -- ^ Message params
     -> PromptCancellable
     -> Map Char (MenuOption a)           -- ^ Menu items
     -> (PromptResult ('Menu a) -> AppM ()) -- ^ Menu promise handler
     -> AppM ()
menu msgPath params cancellable items cb = do
  msg <- Messages.message msgPath params
  let p = mkMenu cancellable items cb
  promptState .= WaitingPrompt msg p

menu_ :: forall (a :: Type).
        [Text]                            -- ^ Message key
      -> PromptCancellable
      -> Map Char (MenuOption a)           -- ^ Menu items
      -> (PromptResult ('Menu a) -> AppM ()) -- ^ Menu promise handler
      -> AppM ()
menu_ msgPath = menu msgPath $ object []

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

attackAt :: Position -> AppM ()
attackAt pos =
  uses entities (entitiesAtPositionWithType @Creature pos) >>= \case
    Empty               -> say_ ["combat", "nothingToAttack"]
    (creature :< Empty) -> attackCreature creature
    creatures ->
      menu_ ["combat", "menu"] Cancellable (entityMenu_ creatures)
      $ \(MenuResult creature) -> attackCreature creature
 where
  attackCreature (creatureID, creature) = do
    charDamage <- use $ character . characterDamage
    let creature' = Creature.damage charDamage creature
        msgParams = object ["creature" A..= creature']
    if Creature.isDead creature'
      then do
        say ["combat", "killed"] msgParams
        entities . at creatureID .= Nothing
      else do
        say ["combat", "hit"] msgParams
        entities . ix creatureID . positioned .= SomeEntity creature'
    stepGame -- TODO

entityMenu_
  :: (Comonad w, Entity entity)
  => [w entity]
  -> Map Char (MenuOption (w entity))
entityMenu_ = mkMenuItems @[_] . map entityMenuItem
  where
    entityMenuItem wentity
      = let entity = extract wentity
      in (entityMenuChar entity, MenuOption (description entity) wentity)
    entityMenuChar entity
      = let ec = entityChar entity ^. char
        in if ec `elem` (['a'..'z'] ++ ['A'..'Z'])
           then ec
           else 'a'

-- entityMenu :: Entity entity => [entity] -> Map Char (MenuOption entity)
-- entityMenu = map (map runIdentity) . entityMenu_ . fmap Identity

--------------------------------------------------------------------------------
