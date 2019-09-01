module Xanthous.App (makeApp) where

import Xanthous.Prelude
import Brick hiding (App, halt, continue)
import qualified Brick
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Input.Events (Event(EvKey))
import Control.Monad.State (get)

import Xanthous.Game
import Xanthous.Game.Draw (drawGame)
import Xanthous.Resource (Name)
import Xanthous.Command
import Xanthous.Data (move)
import Xanthous.Monad

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

startEvent :: AppM ()
startEvent = say ["welcome"]

handleEvent :: BrickEvent Name () -> AppM (Next GameState)
handleEvent (VtyEvent (EvKey k mods))
  | Just command <- commandFromKey k mods
  = handleCommand command
handleEvent _ = continue

handleCommand :: Command -> AppM (Next GameState)
handleCommand Quit = halt
handleCommand (Move dir) = do
  characterPosition %= move dir
  continue
handleCommand _ = error "unimplemented"
