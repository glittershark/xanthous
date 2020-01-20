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
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Vector as V
import           System.Exit
import           System.Directory (doesFileExist)
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
import           Xanthous.Data.Levels (prevLevel, nextLevel)
import qualified Xanthous.Data.Levels as Levels
import           Xanthous.Game
import           Xanthous.Game.State
import           Xanthous.Game.Draw (drawGame)
import           Xanthous.Game.Prompt
import           Xanthous.Monad
import           Xanthous.Resource (Name, Panel(..))
import qualified Xanthous.Messages as Messages
import           Xanthous.Random
import           Xanthous.Util (removeVectorIndex)
import           Xanthous.Util.Inflection (toSentence)
--------------------------------------------------------------------------------
import qualified Xanthous.Entities.Character as Character
import           Xanthous.Entities.Character hiding (pickUpItem)
import           Xanthous.Entities.Item (Item)
import qualified Xanthous.Entities.Item as Item
import           Xanthous.Entities.Creature (Creature)
import qualified Xanthous.Entities.Creature as Creature
import           Xanthous.Entities.Environment
                 (Door, open, locked, GroundMessage(..), Staircase(..))
import           Xanthous.Entities.RawTypes
                 ( edible, eatMessage, hitpointsHealed
                 , attackMessage
                 )
import           Xanthous.Generators
import qualified Xanthous.Generators.CaveAutomata as CaveAutomata
import qualified Xanthous.Generators.Dungeon as Dungeon
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
        whenM (uses sentWelcome not) $ say ["welcome"] =<< use character
        sentWelcome .= True
    Just n ->
      whenM (uses sentWelcome not) $ do
        say ["welcome"] $ object [ "characterName" A..= n ]
        sentWelcome .= True

initLevel :: AppM ()
initLevel = do
  level <- genLevel 0
  entities <>= levelToEntityMap level
  characterPosition .= level ^. levelCharacterPosition

--------------------------------------------------------------------------------

stepGameBy :: Ticks -> AppM ()
stepGameBy ticks = do
  ents <- uses entities EntityMap.toEIDsAndPositioned
  for_ ents $ \(eid, pEntity) -> do
    pEntity' <- step ticks pEntity
    entities . ix eid .= pEntity'

  modify updateCharacterVision

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
handleCommand Quit = confirm_ ["quit", "confirm"] (liftIO exitSuccess) >> continue
handleCommand (Move dir) = do
  newPos <- uses characterPosition $ move dir
  collisionAt newPos >>= \case
    Nothing -> do
      characterPosition .= newPos
      stepGameBy =<< uses (character . speed) (|*| 1)
      describeEntitiesAt newPos
    Just Combat -> attackAt newPos
    Just Stop -> pure ()
  continue

handleCommand PickUp = do
  pos <- use characterPosition
  uses entities (entitiesAtPositionWithType @Item pos) >>= \case
    [] -> say_ ["pickUp", "nothingToPickUp"]
    [item] -> pickUpItem item
    items' ->
      menu_ ["pickUp", "menu"] Cancellable (entityMenu_ items')
      $ \(MenuResult item) -> pickUpItem item
  continue
  where
    pickUpItem (itemID, item) = do
      character %= Character.pickUpItem item
      entities . at itemID .= Nothing
      say ["pickUp", "pickUp"] $ object [ "item" A..= item ]
      stepGameBy 100 -- TODO

handleCommand Drop = do
  selectItemFromInventory_ ["drop", "menu"] Cancellable id
    (say_ ["drop", "nothing"])
    $ \(MenuResult item) -> do
      charPos <- use characterPosition
      entities . EntityMap.atPosition charPos %= (SomeEntity item <|)
      say ["drop", "dropped"] $ object [ "item" A..= item ]
  continue

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
  uses (character . inventory . backpack)
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
            character . inventory . backpack %= removeVectorIndex idx
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

handleCommand ShowInventory = showPanel InventoryPanel >> continue

handleCommand Wield = do
  selectItemFromInventory_ ["wield", "menu"] Cancellable asWieldedItem
    (say_ ["wield", "nothing"])
    $ \(MenuResult item) -> do
      prevItems <- character . inventory . wielded <<.= inRightHand item
      character . inventory . backpack
        <>= fromList (prevItems ^.. wieldedItems . wieldedItem)
      say ["wield", "wielded"] item
  continue

handleCommand Save = do
  -- TODO default save locations / config file?
  prompt_ @'StringPrompt ["save", "location"] Cancellable
    $ \(StringResult filename) -> do
       exists <- liftIO . doesFileExist $ unpack filename
       if exists
       then confirm ["save", "overwrite"] (object ["filename" A..= filename])
            $ doSave filename
       else doSave filename
  continue
  where
    doSave filename = do
      src <- gets saveGame
      lift . liftIO $ do
        writeFile (unpack filename) $ toStrict src
        exitSuccess

handleCommand GoUp = do
  charPos <- use characterPosition
  hasStairs <- uses (entities . EntityMap.atPosition charPos)
              $ elem (SomeEntity UpStaircase)
  if hasStairs
  then uses levels prevLevel >>= \case
    Just levs' -> levels .= levs'
    Nothing ->
      -- TODO in nethack, this leaves the game. Maybe something similar here?
      say_ ["cant", "goUp"]
  else say_ ["cant", "goUp"]

  continue

handleCommand GoDown = do
  charPos <- use characterPosition
  hasStairs <- uses (entities . EntityMap.atPosition charPos)
              $ elem (SomeEntity DownStaircase)

  if hasStairs
  then do
    levs <- use levels
    let newLevelNum = Levels.pos levs + 1
    levs' <- nextLevel (levelToEntityMap <$> genLevel newLevelNum) levs
    cEID <- use characterEntityID
    pCharacter <- entities . at cEID <<.= Nothing
    levels .= levs'
    entities . at cEID .= pCharacter
  else say_ ["cant", "goDown"]

  continue

--

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
  = clearPrompt >> continue
handlePromptEvent _ pr (VtyEvent (EvKey KEnter []))
  = clearPrompt >> submitPrompt pr >> continue

handlePromptEvent _ pr@(Prompt _ SConfirm _ _ _) (VtyEvent (EvKey (KChar 'y') []))
  = clearPrompt >> submitPrompt pr >> continue

handlePromptEvent _ (Prompt _ SConfirm _ _ _) (VtyEvent (EvKey (KChar 'n') []))
  = clearPrompt >> continue

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
  = clearPrompt >> cb (DirectionResult dir) >> continue
handlePromptEvent _ (Prompt _ SDirectionPrompt _ _ _) _ = continue

handlePromptEvent _ (Prompt _ SMenu _ items' cb) (VtyEvent (EvKey (KChar chr) []))
  | Just (MenuOption _ res) <- items' ^. at chr
  = clearPrompt >> cb (MenuResult res) >> continue
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

handlePromptEvent
  _
  (Prompt Cancellable _ _ _ _)
  (VtyEvent (EvKey (KChar 'q') []))
  = clearPrompt >> continue
handlePromptEvent _ _ _ = continue

clearPrompt :: AppM ()
clearPrompt = promptState .= NoPrompt

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

confirm
  :: ToJSON params
  => [Text] -- ^ Message key
  -> params
  -> AppM ()
  -> AppM ()
confirm msgPath params
  = prompt @'Confirm msgPath params Cancellable . const

confirm_ :: [Text] -> AppM () -> AppM ()
confirm_ msgPath = confirm msgPath $ object []

menu :: forall (a :: Type) (params :: Type).
       (ToJSON params)
     => [Text]                            -- ^ Message key
     -> params                            -- ^ Message params
     -> PromptCancellable
     -> Map Char (MenuOption a)           -- ^ Menu items
     -> (PromptResult ('Menu a) -> AppM ()) -- ^ Menu promise handler
     -> AppM ()
menu msgPath params cancellable items' cb = do
  msg <- Messages.message msgPath params
  let p = mkMenu cancellable items' cb
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
    charDamage <- uses character characterDamage
    let creature' = Creature.damage charDamage creature
        msgParams = object ["creature" A..= creature']
    if Creature.isDead creature'
      then do
        say ["combat", "killed"] msgParams
        entities . at creatureID .= Nothing
      else do
        msg <- uses character getAttackMessage
        message msg msgParams
        entities . ix creatureID . positioned .= SomeEntity creature'
    stepGame -- TODO
  getAttackMessage chr =
    case chr ^? inventory . wielded . wieldedItems . wieldableItem of
      Just wi ->
        fromMaybe (Messages.lookup ["combat", "hit", "generic"])
        $ wi ^. attackMessage
      Nothing ->
        Messages.lookup ["combat", "hit", "fists"]

entityMenu_
  :: (Comonad w, Entity entity)
  => [w entity]
  -> Map Char (MenuOption (w entity))
entityMenu_ = mkMenuItems @[_] . map entityMenuItem
  where
    entityMenuItem wentity
      = let entity = extract wentity
      in (entityMenuChar entity, MenuOption (description entity) wentity)


entityMenuChar :: Entity a => a -> Char
entityMenuChar entity
  = let ec = entityChar entity ^. char
    in if ec `elem` (['a'..'z'] ++ ['A'..'Z'])
        then ec
        else 'a'

-- | Prompt with an item to select out of the inventory, remove it from the
-- inventory, and call callback with it
selectItemFromInventory
  :: forall item params.
    (ToJSON params)
  => [Text]            -- ^ Menu message
  -> params            -- ^ Menu message params
  -> PromptCancellable -- ^ Is the menu cancellable?
  -> Prism' Item item  -- ^ Attach some extra information to the item, in a
                      --   recoverable fashion. Prism vs iso so we can discard
                      --   items.
  -> AppM ()            -- ^ Action to take if there are no items matching
  -> (PromptResult ('Menu item) -> AppM ())
  -> AppM ()
selectItemFromInventory msgPath msgParams cancellable extraInfo onEmpty cb =
  uses (character . inventory . backpack)
       (V.mapMaybe $ preview extraInfo)
    >>= \case
      Empty -> onEmpty
      items' ->
        menu msgPath msgParams cancellable (itemMenu items')
        $ \(MenuResult (idx, item)) -> do
          character . inventory . backpack %= removeVectorIndex idx
          cb $ MenuResult item
  where
    itemMenu = mkMenuItems . imap itemMenuItem
    itemMenuItem idx extraInfoItem =
      let item = extraInfo # extraInfoItem
      in ( entityMenuChar item
         , MenuOption (description item) (idx, extraInfoItem))

selectItemFromInventory_
  :: forall item.
    [Text]            -- ^ Menu message
  -> PromptCancellable -- ^ Is the menu cancellable?
  -> Prism' Item item  -- ^ Attach some extra information to the item, in a
                      --   recoverable fashion. Prism vs iso so we can discard
                      --   items.
  -> AppM ()            -- ^ Action to take if there are no items matching
  -> (PromptResult ('Menu item) -> AppM ())
  -> AppM ()
selectItemFromInventory_ msgPath = selectItemFromInventory msgPath ()

-- entityMenu :: Entity entity => [entity] -> Map Char (MenuOption entity)
-- entityMenu = map (map runIdentity) . entityMenu_ . fmap Identity

showPanel :: Panel -> AppM ()
showPanel panel = do
  activePanel ?= panel
  prompt_ @'Continue ["generic", "continue"] Uncancellable
    . const
    $ activePanel .= Nothing

--------------------------------------------------------------------------------

genLevel
  :: Int -- ^ level number
  -> AppM Level
genLevel _num = do
  let dims = Dimensions 80 80
  generator <- choose $ CaveAutomata :| [Dungeon]
  level <- case generator of
    CaveAutomata -> generateLevel SCaveAutomata CaveAutomata.defaultParams dims
    Dungeon -> generateLevel SDungeon Dungeon.defaultParams dims
  characterPosition .= level ^. levelCharacterPosition
  pure $!! level
