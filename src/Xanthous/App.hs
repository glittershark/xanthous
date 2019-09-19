{-# LANGUAGE ViewPatterns #-}
module Xanthous.App (makeApp) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
import           Brick hiding (App, halt, continue, raw)
import qualified Brick
import           Graphics.Vty.Attributes (defAttr)
import           Graphics.Vty.Input.Events (Event(EvKey))
import           Control.Monad.State (get)
import           Control.Monad.State.Class (modify)
import           Data.Aeson (object)
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
import           Xanthous.Monad
import           Xanthous.Resource (Name)
--------------------------------------------------------------------------------
import           Xanthous.Entities.Creature (Creature)
import qualified Xanthous.Entities.Creature as Creature
import qualified Xanthous.Entities.Character as Character
import           Xanthous.Entities.RawTypes (EntityRaw(..))
import           Xanthous.Entities.Raws (raw)
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
  , appHandleEvent = \state event -> runAppM (handleEvent event) state
  , appStartEvent = runAppM $ startEvent >> get
  , appAttrMap = const $ attrMap defAttr []
  }

runAppM :: AppM a -> GameState -> EventM Name a
runAppM appm = fmap fst . runAppT appm

testGormlak :: Creature
testGormlak =
  let Just (Creature gormlak) = raw "gormlak"
  in Creature.newWithType gormlak

startEvent :: AppM ()
startEvent = do
  say_ ["welcome"]
  level <-
    generateLevel SCaveAutomata CaveAutomata.defaultParams
    $ Dimensions 80 80
  entities <>= (SomeEntity <$> level ^. levelWalls)
  entities <>= (SomeEntity <$> level ^. levelItems)
  characterPosition .= level ^. levelCharacterPosition
  modify updateCharacterVision
  -- entities %= EntityMap.insertAt (Position 10 10) (SomeEntity testGormlak)


handleEvent :: BrickEvent Name () -> AppM (Next GameState)
handleEvent (VtyEvent (EvKey k mods))
  | Just command <- commandFromKey k mods
  = do messageHistory %= hideMessage
       handleCommand command
handleEvent _ = continue

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

