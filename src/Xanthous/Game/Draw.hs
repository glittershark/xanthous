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
import           Xanthous.Data.EntityMap (EntityMap, atPosition)
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Game.State
import           Xanthous.Entities.Character
import           Xanthous.Game
                 ( GameState(..)
                 , entities
                 , revealedPositions
                 , characterPosition
                 , character
                 , MessageHistory(..)
                 , messageHistory
                 , GamePromptState(..)
                 , promptState
                 , debugState, allRevealed
                 )
import           Xanthous.Game.Prompt
import           Xanthous.Resource (Name)
import qualified Xanthous.Resource as Resource
import           Xanthous.Orphans ()
--------------------------------------------------------------------------------

cursorPosition :: GameState -> Widget Name -> Widget Name
cursorPosition game
  | WaitingPrompt _ (Prompt _ SPointOnMap (PointOnMapPromptState pos) _ _)
    <- game ^. promptState
  = showCursor Resource.Prompt (pos ^. loc)
  | otherwise
  = showCursor Resource.Character (game ^. characterPosition . loc)

drawMessages :: MessageHistory -> Widget Name
drawMessages = txt . (<> " ") . unwords . oextract

drawPromptState :: GamePromptState m -> Widget Name
drawPromptState NoPrompt = emptyWidget
drawPromptState (WaitingPrompt msg (Prompt _ pt ps pri _)) =
  case (pt, ps, pri) of
    (SStringPrompt, StringPromptState edit, _) ->
      txt msg <+> renderEditor (txt . fold) True edit
    (SDirectionPrompt, DirectionPromptState, _) -> txt msg
    (SContinue, _, _) -> txt msg
    (SMenu, _, menuItems) ->
      txt msg
      <=> foldl' (<=>) emptyWidget (map drawMenuItem $ itoList menuItems)
    _ -> txt msg
  where
    drawMenuItem (chr, MenuOption m _) =
      str ("[" <> pure chr <> "] ") <+> txt m

drawEntities
  :: (Position -> Bool)
    -- ^ Can we render a given position?
  -> EntityMap SomeEntity -- ^ all entities
  -> Widget Name
drawEntities canRenderPos allEnts
  = vBox rows
  where
    entityPositions = EntityMap.positions allEnts
    maxY = fromMaybe 0 $ maximumOf (folded . y) entityPositions
    maxX = fromMaybe 0 $ maximumOf (folded . x) entityPositions
    rows = mkRow <$> [0..maxY]
    mkRow rowY = hBox $ renderEntityAt . flip Position rowY <$> [0..maxX]
    renderEntityAt pos
      | canRenderPos pos
      = let neighbors = EntityMap.neighbors pos allEnts
        in maybe (str " ") (drawWithNeighbors neighbors)
           $ maximumByOf
             (atPosition pos . folded)
             (compare `on` drawPriority)
             allEnts
      | otherwise = str " "

drawMap :: GameState -> Widget Name
drawMap game
  = viewport Resource.MapViewport Both
  . cursorPosition game
  $ drawEntities
    (\pos ->
         (game ^. debugState . allRevealed)
       || (pos `member` (game ^. revealedPositions)))
    -- FIXME: this will break down as soon as creatures can walk around on their
    -- own, since we don't want to render things walking around when the
    -- character can't see them
    (game ^. entities)

drawCharacterInfo :: Character -> Widget Name
drawCharacterInfo ch = txt " " <+> charName <+> charHitpoints
  where
    charName | Just n <- ch ^. characterName
             = txt n <+> txt " "
             | otherwise
             = emptyWidget
    charHitpoints
        = txt "Hitpoints: "
      <+> txt (tshow $ let Hitpoints hp = characterHitpoints ch in hp)

drawGame :: GameState -> [Widget Name]
drawGame game
  = pure
  . withBorderStyle unicode
  $ case game ^. promptState of
       NoPrompt -> drawMessages (game ^. messageHistory)
       _ -> emptyWidget
  <=> drawPromptState (game ^. promptState)
  <=> border (drawMap game)
  <=> drawCharacterInfo (game ^. character)
