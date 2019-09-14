module Xanthous.App (makeApp) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
import           Brick hiding (App, halt, continue, raw)
import qualified Brick
import           Graphics.Vty.Attributes (defAttr)
import           Graphics.Vty.Input.Events (Event(EvKey))
import           Control.Monad.State (get)
import           Control.Monad.Random (getRandom)
--------------------------------------------------------------------------------
import           Xanthous.Command
import           Xanthous.Data
                 ( move
                 , Position(..)
                 , Dimensions'(Dimensions)
                 , Dimensions
                 , positionFromPair
                 )
import           Xanthous.Data.EntityMap (EntityMap)
import           Xanthous.Game
import           Xanthous.Game.Draw (drawGame)
import           Xanthous.Monad
import           Xanthous.Resource (Name)
--------------------------------------------------------------------------------
import           Xanthous.Entities.Creature (Creature)
import qualified Xanthous.Entities.Creature as Creature
import           Xanthous.Entities.RawTypes (EntityRaw(..))
import           Xanthous.Entities.Raws (raw)
import           Xanthous.Entities
import           Xanthous.Generators
import qualified Xanthous.Generators.CaveAutomata as CaveAutomata
import           Xanthous.Generators.LevelContents
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
  (level, charPos) <-
    generateLevel SCaveAutomata CaveAutomata.defaultParams
    $ Dimensions 80 80
  entities <>= level
  characterPosition .= charPos
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
    Nothing -> characterPosition .= newPos
    Just Combat -> undefined
    Just Stop -> pure ()
  continue

handleCommand PreviousMessage = do
  messageHistory %= popMessage
  continue

--------------------------------------------------------------------------------

generateLevel
  :: SGenerator gen
  -> Params gen
  -> Dimensions
  -> AppM (EntityMap SomeEntity, Position)
generateLevel g ps dims = do
  gen <- use randomGen
  let cells = generate g ps dims gen
  _ <- getRandom @_ @Int -- perturb the generator, so we don't get the same level twice
  charPos <- positionFromPair <$> chooseCharacterPosition cells
  let level = SomeEntity <$> cellsToWalls cells
  pure (level, charPos)
