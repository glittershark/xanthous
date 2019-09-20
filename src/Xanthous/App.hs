{-# LANGUAGE ViewPatterns #-}
module Xanthous.App (makeApp) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
import           Brick hiding (App, halt, continue, raw)
import qualified Brick
import           Brick.Widgets.Edit (handleEditorEvent)
import           Graphics.Vty.Attributes (defAttr)
import           Graphics.Vty.Input.Events (Event(EvKey), Key(..))
import           Control.Monad.State (get, state, StateT(..))
import           Data.Coerce
import           Control.Monad.State.Class (modify)
import           Data.Aeson (object, ToJSON)
import qualified Data.Aeson as A
--------------------------------------------------------------------------------
import           Xanthous.Command
import           Xanthous.Data
                 ( move
                 , Dimensions'(Dimensions)
                 , positioned
                 )
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Game
import           Xanthous.Game.Draw (drawGame)
import           Xanthous.Game.Prompt
import           Xanthous.Monad
import           Xanthous.Resource (Name)
import           Xanthous.Messages (message)
--------------------------------------------------------------------------------
import qualified Xanthous.Entities.Character as Character
import           Xanthous.Entities.Character (characterName)
import           Xanthous.Entities
import           Xanthous.Entities.Item (Item)
import           Xanthous.Generators
import qualified Xanthous.Generators.CaveAutomata as CaveAutomata
--------------------------------------------------------------------------------

type App = Brick.App GameState () Name
type AppM a = AppT (EventM Name) a

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

-- testGormlak :: Creature
-- testGormlak =
--   let Just (Creature gormlak) = raw "gormlak"
--   in Creature.newWithType gormlak

startEvent :: AppM ()
startEvent = do
  level <-
    generateLevel SCaveAutomata CaveAutomata.defaultParams
    $ Dimensions 80 80
  entities <>= (SomeEntity <$> level ^. levelWalls)
  entities <>= (SomeEntity <$> level ^. levelItems)
  characterPosition .= level ^. levelCharacterPosition
  modify updateCharacterVision
  prompt_ @'StringPrompt ["character", "namePrompt"] Uncancellable
    $ \(StringResult s) -> do
      character . characterName ?= s
      say ["welcome"] =<< use character

handleEvent :: BrickEvent Name () -> AppM (Next GameState)
handleEvent ev = use promptState >>= \case
  NoPrompt -> handleNoPromptEvent ev
  WaitingPrompt msg pr -> handlePromptEvent msg pr ev


handleNoPromptEvent :: BrickEvent Name () -> AppM (Next GameState)
handleNoPromptEvent (VtyEvent (EvKey k mods))
  | Just command <- commandFromKey k mods
  = do messageHistory %= hideMessage
       handleCommand command
handleNoPromptEvent _ = continue

handleCommand :: Command -> AppM (Next GameState)
handleCommand Quit = halt
handleCommand (Move dir) = do
  newPos <- uses characterPosition $ move dir
  collisionAt newPos >>= \case
    Nothing -> do
      characterPosition .= newPos
      modify updateCharacterVision
    Just Combat -> undefined
    Just Stop -> pure ()
  continue

handleCommand PickUp = do
  pos <- use characterPosition
  ents <- uses entities $ EntityMap.atPositionWithIDs pos
  let items = flip foldMap ents $ \(eid, view positioned -> se) ->
        case downcastEntity @Item se of
          Just item -> [(eid, item)]
          Nothing   -> []
  case items of
    [] -> say_ ["items", "nothingToPickUp"]
    [(itemID, item)] -> do
      character %= Character.pickUpItem item
      entities . at itemID .= Nothing
      say ["items", "pickUp"] $ object [ "item" A..= item ]
    _ -> undefined
  continue

handleCommand PreviousMessage = do
  messageHistory %= popMessage
  continue

handlePromptEvent
  :: Text -- ^ Prompt message
  -> Prompt (AppT Identity)
  -> BrickEvent Name ()
  -> AppM (Next GameState)
handlePromptEvent _ (Prompt Cancellable _ _ _) (VtyEvent (EvKey KEsc [])) = do
  promptState .= NoPrompt
  continue
handlePromptEvent _ pr (VtyEvent (EvKey KEnter [])) = do
  () <- state . coerce $ submitPrompt pr
  promptState .= NoPrompt
  continue
handlePromptEvent
  msg
  (Prompt c SStringPrompt (StringPromptState edit) cb)
  (VtyEvent ev)
  = do
    edit' <- lift $ handleEditorEvent ev edit
    let prompt' = Prompt c SStringPrompt (StringPromptState edit') cb
    promptState .= WaitingPrompt msg prompt'
    continue
handlePromptEvent _ _ _ = undefined

prompt
  :: forall (pt :: PromptType) (params :: Type).
    (ToJSON params, SingPromptType pt)
  => [Text]                     -- ^ Message key
  -> params                     -- ^ Message params
  -> PromptCancellable
  -> (PromptResult pt -> AppT Identity ()) -- ^ Prompt promise handler
  -> AppM ()
prompt msgPath params cancellable cb = do
  let pt = singPromptType @pt
  msg <- message msgPath params
  let p = mkPrompt cancellable pt cb
  promptState .= WaitingPrompt msg p

prompt_
  :: forall (pt :: PromptType) .
    (SingPromptType pt)
  => [Text] -- ^ Message key
  -> PromptCancellable
  -> (PromptResult pt -> AppT Identity ()) -- ^ Prompt promise handler
  -> AppM ()
prompt_ msg = prompt msg $ object []
