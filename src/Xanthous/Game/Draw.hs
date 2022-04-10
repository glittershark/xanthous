--------------------------------------------------------------------------------
module Xanthous.Game.Draw
  ( drawGame
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Brick hiding (loc, on)
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Edit
import           Control.Monad.State.Lazy (evalState)
import           Control.Monad.State.Class ( get, MonadState, gets )
--------------------------------------------------------------------------------
import           Xanthous.Data
import           Xanthous.Data.App (ResourceName, Panel(..))
import qualified Xanthous.Data.App as Resource
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Game.State
import           Xanthous.Entities.Common (Wielded(..), wielded, backpack)
import           Xanthous.Entities.Character
import           Xanthous.Entities.Item (Item)
import           Xanthous.Game
                 ( characterPosition
                 , character
                 , revealedEntitiesAtPosition
                 )
import           Xanthous.Game.Prompt
import           Xanthous.Orphans ()
import Brick.Widgets.Center (hCenter)
import Xanthous.Command (Keybinding (..), keybindings, Command, commandIsHidden)
import Graphics.Vty.Input.Events (Modifier(..))
import Graphics.Vty.Input (Key(..))
import Brick.Widgets.Table
--------------------------------------------------------------------------------

cursorPosition :: GameState -> Widget ResourceName -> Widget ResourceName
cursorPosition game
  | WaitingPrompt _ (Prompt _ _ (preview promptStatePosition -> Just pos) _ _)
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
    (SStringPrompt, StringPromptState edit, mDef) ->
      txt msg
      <+> txt (maybe "" (\def -> "(default: " <> def <> ") ") mDef)
      <+> renderEditor (txt . fold) True edit
    (SDirectionPrompt, DirectionPromptState, _) -> txtWrap msg
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

drawHelpPanel :: Widget ResourceName
drawHelpPanel
  = txtWrap "To move in a direction or attack, use vi keys (hjklyubn):"
  <=> txt " "
  <=> hCenter keyStar
  <=> txt " "
  <=> cmds
  where
    keyStar
      =   txt "y k u"
      <=> txt " \\|/"
      <=> txt "h-.-l"
      <=> txt " /|\\"
      <=> txt "b j n"

    cmds
      = renderTable
      . alignRight 0
      . setDefaultRowAlignment AlignTop
      . surroundingBorder False
      . rowBorders False
      . columnBorders False
      . table $ help <&> \(key, cmd) -> [ txt $ key <> " : "
                                       , hLimitPercent 100 $ txtWrap cmd]

    help =
      extraHelp <>
      keybindings
        ^.. ifolded
          . filtered (not . commandIsHidden)
          . withIndex
          . to (bimap displayKeybinding displayCommand)
    extraHelp
      = [("Shift-Dir", "Auto-move")]

    displayCommand = tshow @Command
    displayKeybinding (Keybinding k mods) = foldMap showMod mods <> showKey k

    showMod MCtrl  = "Ctrl-"
    showMod MShift = "Shift-"
    showMod MAlt   = "Alt-"
    showMod MMeta  = "Meta-"

    showKey (KChar c) = pack [c]
    showKey KEsc = "<Esc>"
    showKey KBS = "<Backspace>"
    showKey KEnter = "<Enter>"
    showKey KLeft = "<Left>"
    showKey KRight = "<Right>"
    showKey KUp = "<Up>"
    showKey KDown = "<Down>"
    showKey KUpLeft = "<UpLeft>"
    showKey KUpRight = "<UpRight>"
    showKey KDownLeft = "<DownLeft>"
    showKey KDownRight = "<DownRight>"
    showKey KCenter = "<Center>"
    showKey (KFun n) = "<F" <> tshow n <> ">"
    showKey KBackTab = "<BackTab>"
    showKey KPrtScr = "<PrtScr>"
    showKey KPause = "<Pause>"
    showKey KIns = "<Ins>"
    showKey KHome = "<Home>"
    showKey KPageUp = "<PageUp>"
    showKey KDel = "<Del>"
    showKey KEnd = "<End>"
    showKey KPageDown = "<PageDown>"
    showKey KBegin = "<Begin>"
    showKey KMenu = "<Menu>"

drawPanel :: GameState -> Panel -> Widget ResourceName
drawPanel game panel
  = border
  . hLimit 35
  . viewport (Resource.Panel panel) Vertical
  $ case panel of
      HelpPanel -> drawHelpPanel
      InventoryPanel -> drawInventoryPanel game
      ItemDescriptionPanel desc -> txtWrap desc

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
