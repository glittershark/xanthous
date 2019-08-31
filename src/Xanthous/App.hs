module Xanthous.App (makeApp) where

import Xanthous.Prelude
import Brick hiding (App)
import qualified Brick
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Input.Events (Event(EvResize, EvKey))

import Xanthous.Game
import Xanthous.Game.Draw (drawGame)
import Xanthous.Resource (Name)
import Xanthous.Command
import Xanthous.Data (move)

type App = Brick.App GameState () Name

makeApp :: IO App
makeApp = pure $ Brick.App
  { appDraw = drawGame
  , appChooseCursor = const headMay
  , appHandleEvent = handleEvent
  , appStartEvent = pure
  , appAttrMap = const $ attrMap defAttr []
  }

handleEvent :: GameState -> BrickEvent Name () -> EventM Name (Next GameState)
handleEvent game (VtyEvent (EvKey k mods))
  | Just command <- commandFromKey k mods
  = handleCommand command game
handleEvent game _ = continue game

handleCommand :: Command -> GameState -> EventM Name (Next GameState)
handleCommand Quit = halt
handleCommand (Move dir) = continue . (characterPosition %~ move dir)
handleCommand _ = undefined
