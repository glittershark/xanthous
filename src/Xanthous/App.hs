module Xanthous.App (makeApp) where

import Xanthous.Prelude
import Brick hiding (App)
import qualified Brick
import Graphics.Vty.Attributes (defAttr)

import Xanthous.Game
import Xanthous.Game.Draw (drawGame)
import Xanthous.Resource (Name)

type App = Brick.App GameState () Name

makeApp :: IO App
makeApp = pure $ Brick.App
  { appDraw = drawGame
  , appChooseCursor = const headMay
  , appHandleEvent = resizeOrQuit
  , appStartEvent = pure
  , appAttrMap = const $ attrMap defAttr []
  }
