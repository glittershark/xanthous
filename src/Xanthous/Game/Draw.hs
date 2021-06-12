--------------------------------------------------------------------------------
module Xanthous.Game.Draw
  ( drawGame
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
import           Brick hiding (loc, on)
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Edit
--------------------------------------------------------------------------------
import           Xanthous.Data
import           Xanthous.Data.App (ResourceName, Panel(..))
import qualified Xanthous.Data.App as Resource
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Game.State
import           Xanthous.Entities.Character
import           Xanthous.Entities.Item (Item)
import           Xanthous.Game
                 ( characterPosition
                 , character
                 , revealedEntitiesAtPosition
                 )
import           Xanthous.Game.Prompt
import           Xanthous.Orphans ()
import Control.Monad.State.Lazy (evalState)
import Control.Monad.State.Class ( get, MonadState, gets )
--------------------------------------------------------------------------------

cursorPosition :: GameState -> Widget ResourceName -> Widget ResourceName
cursorPosition game
  | WaitingPrompt _ (Prompt _ SPointOnMap (PointOnMapPromptState pos) _ _)
    <- game ^. promptState
  = showCursor Resource.Prompt (pos ^. loc)
  | otherwise
  = showCursor Resource.Character (game ^. characterPosition . loc)

drawMessages :: MessageHistory -> Widget ResourceName
drawMessages = txtWrap . (<> " ") . unwords . reverse . oextract

drawPromptState :: GamePromptState m -> Widget ResourceName
drawPromptState NoPrompt = emptyWidget
drawPromptState (WaitingPrompt msg (Prompt _ pt ps pri _)) =
  case (pt, ps, pri) of
    (SStringPrompt, StringPromptState edit, _) ->
      txtWrap msg <+> txt " " <+> renderEditor (txt . fold) True edit
    (SDirectionPrompt, DirectionPromptState, _) -> txtWrap msg
    (SContinue, _, _) -> txtWrap msg
    (SMenu, _, menuItems) ->
      txtWrap msg
      <=> foldl' (<=>) emptyWidget (map drawMenuItem $ itoList menuItems)
    _ -> txtWrap msg
  where
    drawMenuItem (chr, MenuOption m _) =
      str ("[" <> pure chr <> "] ") <+> txtWrap m

drawEntities
  :: forall m. MonadState GameState m
  => m (Widget ResourceName)
drawEntities = do
  allEnts <- use entities
  let entityPositions = EntityMap.positions allEnts
      maxY = fromMaybe 0 $ maximumOf (folded . y) entityPositions
      maxX = fromMaybe 0 $ maximumOf (folded . x) entityPositions
      rows = traverse mkRow [0..maxY]
      mkRow rowY = hBox <$> traverse (renderEntityAt . flip Position rowY) [0..maxX]
      renderEntityAt pos
        = renderTopEntity pos <$> revealedEntitiesAtPosition pos
      renderTopEntity pos ents
        = let neighbors = EntityMap.neighbors pos allEnts
          in maybe (str " ") (drawWithNeighbors neighbors)
             $ maximumBy (compare `on` drawPriority)
             <$> fromNullable ents
  vBox <$> rows

drawMap :: MonadState GameState m => m (Widget ResourceName)
drawMap = do
  cursorPos <- gets cursorPosition
  viewport Resource.MapViewport Both . cursorPos <$> drawEntities

bullet :: Char
bullet = '•'

drawInventoryPanel :: GameState -> Widget ResourceName
drawInventoryPanel game
  =   drawWielded  (game ^. character . inventory . wielded)
  <=> drawBackpack (game ^. character . inventory . backpack)
  where
    drawWielded (Hands Nothing Nothing) = emptyWidget
    drawWielded (DoubleHanded i) =
      txtWrap $ "You are holding " <> description i <> " in both hands"
    drawWielded (Hands l r) = drawHand "left" l <=> drawHand "right" r
    drawHand side = maybe emptyWidget $ \i ->
      txtWrap ( "You are holding "
              <> description i
              <> " in your " <> side <> " hand"
              )
      <=> txt " "

    drawBackpack :: Vector Item -> Widget ResourceName
    drawBackpack Empty = txtWrap "Your backpack is empty right now."
    drawBackpack backpackItems
      = txtWrap ( "You are currently carrying the following items in your "
                <> "backpack:")
        <=> txt " "
        <=> foldl' (<=>) emptyWidget
            (map
              (txtWrap . ((bullet <| " ") <>) . description)
              backpackItems)


drawPanel :: GameState -> Panel -> Widget ResourceName
drawPanel game panel
  = border
  . hLimit 35
  . viewport (Resource.Panel panel) Vertical
  . case panel of
      InventoryPanel -> drawInventoryPanel
  $ game

drawCharacterInfo :: Character -> Widget ResourceName
drawCharacterInfo ch = txt " " <+> charName <+> charHitpoints
  where
    charName | Just n <- ch ^. characterName
             = txt $ n <> " "
             | otherwise
             = emptyWidget
    charHitpoints
        = txt "Hitpoints: "
      <+> txt (tshow $ let Hitpoints hp = characterHitpoints ch in hp)

drawGame :: GameState -> [Widget ResourceName]
drawGame = evalState $ do
  game <- get
  drawnMap <- drawMap
  pure
    . pure
    . withBorderStyle unicode
    $ case game ^. promptState of
        NoPrompt -> drawMessages (game ^. messageHistory)
        _ -> emptyWidget
    <=> drawPromptState (game ^. promptState)
    <=>
    (maybe emptyWidget (drawPanel game) (game ^. activePanel)
    <+> border drawnMap
    )
    <=> drawCharacterInfo (game ^. character)
