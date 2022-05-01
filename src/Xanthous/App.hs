{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards      #-}
--------------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Xanthous.App
  ( makeApp
  , RunType(..)
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
import           Brick hiding (App, halt, continue, raw)
import qualified Brick
import           Graphics.Vty.Attributes (defAttr)
import           Graphics.Vty.Input.Events (Event(EvKey))
import           Control.Monad.State (get, gets)
import           Control.Monad.State.Class (modify)
import           Data.Aeson (object, ToJSON)
import qualified Data.Aeson as A
import qualified Data.Vector as V
import           System.Exit
import           System.Directory (doesFileExist)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Vector.Lens (toVectorOf)
--------------------------------------------------------------------------------
import           Xanthous.App.Common
import           Xanthous.App.Time
import           Xanthous.App.Prompt
import           Xanthous.App.Autocommands
import           Xanthous.Command
import           Xanthous.Data
                 ( move
                 , Dimensions'(Dimensions)
                 , positioned
                 , position
                 , Position
                 , (|*|)
                 , Tiles(..), Hitpoints, fromScalar
                 )
import           Xanthous.Data.App (ResourceName, Panel(..), AppEvent(..))
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Data.Levels (prevLevel, nextLevel)
import qualified Xanthous.Data.Levels as Levels
import           Xanthous.Data.Entities (blocksObject)
import           Xanthous.Game
import           Xanthous.Game.State
import           Xanthous.Game.Env
import           Xanthous.Game.Draw (drawGame)
import           Xanthous.Game.Prompt hiding (Fire)
import qualified Xanthous.Messages as Messages
import           Xanthous.Random
import           Xanthous.Util (removeVectorIndex, useListOf)
import           Xanthous.Util.Inflection (toSentence)
import           Xanthous.Physics (throwDistance, bluntThrowDamage)
import           Xanthous.Data.EntityMap.Graphics (lineOfSight)
import           Xanthous.Data.EntityMap (EntityID)
--------------------------------------------------------------------------------
import           Xanthous.Entities.Common
                 ( InventoryPosition, describeInventoryPosition, backpack
                 , wieldableItem, wieldedItems, wielded, itemsWithPosition
                 , removeItemFromPosition, asWieldedItem
                 , wieldedItem, items, Hand (..), describeHand, wieldInHand
                 , WieldedItem, Wielded (..)
                 )
import qualified Xanthous.Entities.Character as Character
import           Xanthous.Entities.Character hiding (pickUpItem)
import           Xanthous.Entities.Item (Item, weight)
import qualified Xanthous.Entities.Item as Item
import           Xanthous.Entities.Creature (Creature)
import qualified Xanthous.Entities.Creature as Creature
import           Xanthous.Entities.Environment
                 (Door, open, closed, locked, GroundMessage(..), Staircase(..))
import           Xanthous.Entities.RawTypes
                 ( edible, eatMessage, hitpointsHealed
                 , attackMessage
                 )
import           Xanthous.Generators.Level
import qualified Xanthous.Generators.Level.CaveAutomata as CaveAutomata
import qualified Xanthous.Generators.Level.Dungeon as Dungeon
--------------------------------------------------------------------------------

type App = Brick.App GameState AppEvent ResourceName

data RunType = NewGame | LoadGame FilePath
  deriving stock (Eq)

makeApp :: GameEnv -> RunType -> IO App
makeApp env rt = pure $ Brick.App
  { appDraw = drawGame
  , appChooseCursor = const headMay
  , appHandleEvent = \game event -> runAppM (handleEvent event) env game
  , appStartEvent = case rt of
      NewGame -> runAppM (startEvent >> get) env
      LoadGame save -> pure . (savefile ?~ save)
  , appAttrMap = const $ attrMap defAttr []
  }

runAppM :: AppM a -> GameEnv -> GameState -> EventM ResourceName a
runAppM appm ge = fmap fst . runAppT appm ge

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
  level <- genLevel 0
  entities <>= levelToEntityMap level
  characterPosition .= level ^. levelCharacterPosition

--------------------------------------------------------------------------------

handleEvent :: BrickEvent ResourceName AppEvent -> AppM (Next GameState)
handleEvent ev = use promptState >>= \case
  NoPrompt -> handleNoPromptEvent ev
  WaitingPrompt msg pr -> handlePromptEvent msg pr ev


handleNoPromptEvent :: BrickEvent ResourceName AppEvent -> AppM (Next GameState)
handleNoPromptEvent (VtyEvent (EvKey k mods))
  | Just command <- commandFromKey k mods
  = do messageHistory %= nextTurn
       cancelAutocommand
       handleCommand command
handleNoPromptEvent (AppEvent AutoContinue) = do
  preuse (autocommand . _ActiveAutocommand . _1) >>= traverse_ autoStep
  continue
handleNoPromptEvent _ = continue

handleCommand :: Command -> AppM (Next GameState)
handleCommand Quit = confirm_ ["quit", "confirm"] (liftIO exitSuccess) >> continue

handleCommand Help = showPanel HelpPanel >> continue

handleCommand (Move dir) = do
  newPos <- uses characterPosition $ move dir
  collisionAt newPos >>= \case
    Nothing -> do
      characterPosition .= newPos
      stepGameBy =<< uses (character . speed) (|*| Tiles 1)
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
  takeItemFromInventory_ ["drop", "menu"] Cancellable id
    (say_ ["drop", "nothing"])
    $ \(MenuResult item) -> do
      entitiesAtCharacter %= (SomeEntity item <|)
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
         | all (view $ _2 . open) doors   -> say_ ["open", "alreadyOpen"]
         | otherwise -> do
             for_ doors $ \(eid, _) ->
               entities . ix eid . positioned . _SomeEntity . open .= True
             say_ ["open", "success"]
      pure ()
  stepGame -- TODO
  continue

handleCommand Close = do
  prompt_ @'DirectionPrompt ["close", "prompt"] Cancellable
    $ \(DirectionResult dir) -> do
      pos <- move dir <$> use characterPosition
      (nonDoors, doors) <- uses entities
        $ partitionEithers
        . toList
        . map ( (matching . aside $ _SomeEntity @Door)
              . over _2 (view positioned)
              )
        . EntityMap.atPositionWithIDs pos
      if | null doors -> say_ ["close", "nothingToClose"]
         | all (view $ _2 . closed) doors -> say_ ["close", "alreadyClosed"]
         | any (view blocksObject . entityAttributes . snd) nonDoors ->
           say ["close", "blocked"]
           $ object [ "entityDescriptions"
                      A..= ( toSentence
                           . map description
                           . filter (view blocksObject . entityAttributes)
                           . map snd
                           ) nonDoors
                    , "blockOrBlocks"
                      A..= ( if length nonDoors == 1
                             then "blocks"
                             else "block"
                           :: Text)
                    ]
         | otherwise -> do
             for_ doors $ \(eid, _) ->
               entities . ix eid . positioned . _SomeEntity . closed .= True
             for_ nonDoors $ \(eid, _) ->
               entities . ix eid . position %= move dir
             say_ ["close", "success"]
      pure ()
  stepGame -- TODO
  continue

handleCommand Look = do
  prompt_ @'PointOnMap ["look", "prompt"] Cancellable
    $ \(PointOnMapResult pos) -> revealedEntitiesAtPosition pos >>= \case
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

handleCommand DescribeInventory = do
  selectItemFromInventory_ ["inventory", "describe", "select"] Cancellable id
    (say_ ["inventory", "describe", "nothing"])
    $ \(MenuResult (invPos, item)) -> showPanel . ItemDescriptionPanel
        $ Item.fullDescription item
        <> "\n\n" <> describeInventoryPosition invPos
  continue


handleCommand Wield = do
  hs <- use $ character . inventory . wielded
  selectItem $ \(MenuResult (item :: WieldedItem)) -> do
    selectHand hs $ \(MenuResult hand) -> do
      prevItems <- character . inventory . wielded %%= wieldInHand hand item
      character . inventory . backpack
        <>= fromList (map (view wieldedItem) prevItems)
      say ["wield", "wielded"] $ object [ "item" A..= item
                                        , "hand" A..= describeHand hand
                                        ]
  continue
  where
    selectItem =
      takeItemFromInventory_ ["wield", "menu"] Cancellable asWieldedItem
        (say_ ["wield", "nothing"])
    selectHand hs = menu_ ["wield", "hand"] Cancellable $ handsMenu hs
    itemsInHand (Hands i _) LeftHand       = toList i
    itemsInHand (DoubleHanded _) LeftHand  = []
    itemsInHand (Hands _ i) RightHand      = toList i
    itemsInHand (DoubleHanded _) RightHand = []
    itemsInHand (Hands l r) BothHands      = toList l <> toList r
    itemsInHand (DoubleHanded i) BothHands = [i]
    describeItems [] = ""
    describeItems is
      = " (currently holding "
      <> (intercalate " and" $ map (view $ wieldedItem . to description) is)
      <> ")"
    handsMenu hs = mapFromList
      . map (second $ \hand ->
                MenuOption
                ( describeHand hand
                <> describeItems (itemsInHand hs hand)
                )
                hand
            )
      $ [ ('l', LeftHand)
        , ('r', RightHand)
        , ('b', BothHands)
        ]

handleCommand Fire = do
  selectItemFromInventory_ ["fire", "menu"] Cancellable id
    (say_ ["fire", "nothing"])
    $ \(MenuResult (invPos, item)) ->
      let wt = weight item
          dist = throwDistance wt
          dam = bluntThrowDamage wt
      in if dist < fromScalar 1
         then say_ ["fire", "zeroRange"]
         else firePrompt_ ["fire", "target"] Cancellable dist $
          \(FireResult targetPos) -> do
              charPos <- use characterPosition
              mTarget <- uses entities $ firstEnemy . lineOfSight charPos targetPos
              case mTarget of
                Just target -> do
                  creature' <- damageCreature target dam
                  unless (Creature.isDead creature') $
                    let msgPath = ["fire", "fired"] <> [if dam == 0
                                                        then "noDamage"
                                                        else "someDamage"]
                    in say msgPath $ object [ "item" A..= item
                                            , "creature" A..= creature'
                                            ]
                Nothing ->
                  say ["fire", "fired", "noTarget"] $ object [ "item" A..= item ]
              character . inventory %= removeItemFromPosition invPos item
              entities . EntityMap.atPosition targetPos %= (SomeEntity item <|)
              stepGame -- TODO(grfn): should this be based on distance?
  continue
  where
    firstEnemy
      :: [(Position, Vector (EntityID, SomeEntity))]
      -> Maybe (EntityID, Creature)
    firstEnemy los =
      let enemies = los >>= \(_, es) -> toList $ headMay es
      in enemies ^? folded . below _SomeEntity

handleCommand Save =
  view (config . disableSaving) >>= \case
    True -> say_ ["save", "disabled"] >> continue
    False -> do
      -- TODO default save locations / config file?
      use savefile >>= \case
        Just filepath ->
          stringPromptWithDefault_
            ["save", "location"]
            Cancellable
            (pack filepath)
            promptCallback
        Nothing -> prompt_ @'StringPrompt ["save", "location"] Cancellable promptCallback
      continue
      where
        promptCallback :: PromptResult 'StringPrompt -> AppM ()
        promptCallback (StringResult filename) = do
          sf <- use savefile
          exists <- liftIO . doesFileExist $ unpack filename
          if exists && sf /= Just (unpack filename)
          then confirm ["save", "overwrite"] (object ["filename" A..= filename])
              $ doSave filename
          else doSave filename
        doSave filename = do
          src <- gets saveGame
          lift . liftIO $ do
            writeFile (unpack filename) $ toStrict src
            exitSuccess

handleCommand GoUp = do
  hasStairs <- uses entitiesAtCharacter $ elem (SomeEntity UpStaircase)
  if hasStairs
  then uses levels prevLevel >>= \case
    Just levs' -> do
      cEID <- use characterEntityID
      pCharacter <- entities . at cEID <<.= Nothing
      levels .= levs'
      charPos <- use characterPosition
      entities . at cEID .= pCharacter
      characterPosition .= charPos
    Nothing ->
      -- TODO in nethack, this leaves the game. Maybe something similar here?
      say_ ["cant", "goUp"]
  else say_ ["cant", "goUp"]

  continue

handleCommand GoDown = do
  hasStairs <- uses entitiesAtCharacter $ elem (SomeEntity DownStaircase)

  if hasStairs
  then do
    levs <- use levels
    let newLevelNum = Levels.pos levs + 1
    levs' <- nextLevel (levelToGameLevel <$> genLevel newLevelNum) levs
    cEID <- use characterEntityID
    pCharacter <- entities . at cEID <<.= Nothing
    levels .= levs'
    entities . at cEID .= pCharacter
    characterPosition .= extract levs' ^. upStaircasePosition
  else say_ ["cant", "goDown"]

  continue

handleCommand (StartAutoMove dir) = do
  runAutocommand $ AutoMove dir
  continue

handleCommand Rest = do
  say_ ["autocommands", "resting"]
  runAutocommand AutoRest
  continue

--

handleCommand ToggleRevealAll = do
  val <- debugState . allRevealed <%= not
  say ["debug", "toggleRevealAll"] $ object [ "revealAll" A..= val ]
  continue

--------------------------------------------------------------------------------
attackAt :: Position -> AppM ()
attackAt pos =
  uses entities (entitiesAtPositionWithType @Creature pos) >>= \case
    Empty               -> say_ ["combat", "nothingToAttack"]
    (creature :< Empty) -> attackCreature creature
    creatures ->
      menu_ ["combat", "menu"] Cancellable (entityMenu_ creatures)
      $ \(MenuResult creature) -> attackCreature creature
 where
  attackCreature creature = do
    charDamage <- uses character characterDamage
    creature' <- damageCreature creature charDamage
    msg <- uses character getAttackMessage
    unless (Creature.isDead creature')
      . message msg $ object ["creature" A..= creature']
    whenM (uses character $ isNothing . weapon) handleFists
    stepGame
  weapon chr = chr ^? inventory . wielded . wieldedItems . wieldableItem
  getAttackMessage chr =
    case weapon chr of
      Just wi ->
        fromMaybe (Messages.lookup ["combat", "hit", "generic"])
        $ wi ^. attackMessage
      Nothing ->
        Messages.lookup ["combat", "hit", "fists"]

  handleFists = do
    damageChance <- use $ character . body . knuckles . to fistDamageChance
    whenM (chance damageChance) $ do
      damageAmount <- use $ character . body . knuckles . to fistfightingDamage
      say_ [ "combat" , if damageAmount > 1
                        then "fistExtraSelfDamage"
                        else "fistSelfDamage" ]
      character %= Character.damage damageAmount
      character . body . knuckles %= damageKnuckles

damageCreature :: (EntityID, Creature) -> Hitpoints -> AppM Creature
damageCreature (creatureID, creature) dam = do
  let creature' = Creature.damage dam creature
      msgParams = object ["creature" A..= creature']
  if Creature.isDead creature'
    then do
      say ["combat", "killed"] msgParams
      floorItems <- useListOf
                   $ entities
                   . ix creatureID
                   . positioned
                   . _SomeEntity @Creature
                   . inventory
                   . items
      mCreaturePos <- preuse $ entities . ix creatureID . position
      entities . at creatureID .= Nothing
      for_ mCreaturePos $ \creaturePos ->
        entities . EntityMap.atPosition creaturePos
          %= (<> fromList (SomeEntity <$> floorItems))
    else entities . ix creatureID . positioned .= SomeEntity creature'
  pure creature'


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

-- | Prompt with an item to select out of the inventory and call callback with
-- it
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
  -> (PromptResult ('Menu (InventoryPosition, item)) -> AppM ())
  -> AppM ()
selectItemFromInventory msgPath msgParams cancellable extraInfo onEmpty cb = do
  uses (character . inventory)
       (V.mapMaybe (_2 $ preview extraInfo) . toVectorOf itemsWithPosition)
    >>= \case
      Empty -> onEmpty
      items' -> menu msgPath msgParams cancellable (itemMenu items') cb
  where
    itemMenu = mkMenuItems . map itemMenuItem
    itemMenuItem (invPos, extraInfoItem) =
      let item = extraInfo # extraInfoItem
      in ( entityMenuChar item
         , MenuOption
           (description item <> " (" <> describeInventoryPosition invPos <> ")")
           (invPos, extraInfoItem)
         )

-- | Prompt with an item to select out of the inventory and call callback with
-- it
selectItemFromInventory_
  :: forall item.
    [Text]            -- ^ Menu message
  -> PromptCancellable -- ^ Is the menu cancellable?
  -> Prism' Item item  -- ^ Attach some extra information to the item, in a
                      --   recoverable fashion. Prism vs iso so we can discard
                      --   items.
  -> AppM ()            -- ^ Action to take if there are no items matching
  -> (PromptResult ('Menu (InventoryPosition, item)) -> AppM ())
  -> AppM ()
selectItemFromInventory_ msgPath = selectItemFromInventory msgPath ()

-- | Prompt with an item to select out of the inventory, remove it from the
-- inventory, and call callback with it
takeItemFromInventory
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
takeItemFromInventory msgPath msgParams cancellable extraInfo onEmpty cb =
  selectItemFromInventory msgPath msgParams cancellable extraInfo onEmpty
    $ \(MenuResult (invPos, item)) -> do
      character . inventory
        %= removeItemFromPosition invPos (item ^. re extraInfo)
      cb $ MenuResult item

takeItemFromInventory_
  :: forall item.
    [Text]            -- ^ Menu message
  -> PromptCancellable -- ^ Is the menu cancellable?
  -> Prism' Item item  -- ^ Attach some extra information to the item, in a
                      --   recoverable fashion. Prism vs iso so we can discard
                      --   items.
  -> AppM ()            -- ^ Action to take if there are no items matching
  -> (PromptResult ('Menu item) -> AppM ())
  -> AppM ()
takeItemFromInventory_ msgPath = takeItemFromInventory msgPath ()

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
  :: Word -- ^ Level number, starting at 0
  -> AppM Level
genLevel num = do
  let dims = Dimensions 80 80
  generator <- choose $ CaveAutomata :| [Dungeon]
  let
    doGen = case generator of
      CaveAutomata -> generateLevel SCaveAutomata CaveAutomata.defaultParams
      Dungeon -> generateLevel SDungeon Dungeon.defaultParams
  level <- doGen dims num
  pure $!! level

levelToGameLevel :: Level -> GameLevel
levelToGameLevel level =
  let _levelEntities = levelToEntityMap level
      _upStaircasePosition = level ^. levelCharacterPosition
      _levelRevealedPositions = mempty
  in GameLevel {..}
