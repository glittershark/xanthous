{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
module Xanthous.App.Prompt
  ( handlePromptEvent
  , clearPrompt
  , prompt
  , prompt_
  , stringPromptWithDefault
  , stringPromptWithDefault_
  , confirm_
  , confirm
  , menu
  , menu_
  , firePrompt_
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Brick (BrickEvent(..), Next)
import           Brick.Widgets.Edit (handleEditorEvent)
import           Data.Aeson (ToJSON, object)
import           Graphics.Vty.Input.Events (Event(EvKey), Key(..))
--------------------------------------------------------------------------------
import           Xanthous.App.Common
import           Xanthous.Data (move, Tiles, Position, positioned, _Position)
import qualified Xanthous.Data as Data
import           Xanthous.Command (directionFromChar)
import           Xanthous.Data.App (ResourceName, AppEvent)
import           Xanthous.Game.Prompt
import           Xanthous.Game.State
import qualified Xanthous.Messages as Messages
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Entities.Creature (creatureType, Creature)
import           Xanthous.Entities.RawTypes (hostile)
import qualified Linear.Metric as Metric
--------------------------------------------------------------------------------

handlePromptEvent
  :: Text -- ^ Prompt message
  -> Prompt AppM
  -> BrickEvent ResourceName AppEvent
  -> AppM (Next GameState)

handlePromptEvent _ (Prompt Cancellable _ _ _ _) (VtyEvent (EvKey KEsc []))
  = clearPrompt >> continue
handlePromptEvent _ pr (VtyEvent (EvKey KEnter []))
  = clearPrompt >> submitPrompt pr >> continue

handlePromptEvent _ pr@(Prompt _ SConfirm _ _ _) (VtyEvent (EvKey (KChar 'y') []))
  = clearPrompt >> submitPrompt pr >> continue

handlePromptEvent _ (Prompt _ SConfirm _ _ _) (VtyEvent (EvKey (KChar 'n') []))
  = clearPrompt >> continue

handlePromptEvent
  msg
  (Prompt c SStringPrompt (StringPromptState edit) pri cb)
  (VtyEvent ev)
  = do
    edit' <- lift $ handleEditorEvent ev edit
    let prompt' = Prompt c SStringPrompt (StringPromptState edit') pri cb
    promptState .= WaitingPrompt msg prompt'
    continue

handlePromptEvent _ (Prompt _ SDirectionPrompt _ _ cb)
  (VtyEvent (EvKey (KChar (directionFromChar -> Just dir)) []))
  = clearPrompt >> cb (DirectionResult dir) >> continue
handlePromptEvent _ (Prompt _ SDirectionPrompt _ _ _) _ = continue

handlePromptEvent _ (Prompt _ SMenu _ items' cb) (VtyEvent (EvKey (KChar chr) []))
  | Just (MenuOption _ res) <- items' ^. at chr
  = clearPrompt >> cb (MenuResult res) >> continue
  | otherwise
  = continue

handlePromptEvent
  msg
  (Prompt c SPointOnMap (PointOnMapPromptState pos) pri cb)
  (VtyEvent (EvKey (KChar (directionFromChar -> Just dir)) []))
  = let pos' = move dir pos
        prompt' = Prompt c SPointOnMap (PointOnMapPromptState pos') pri cb
    in promptState .= WaitingPrompt msg prompt'
       >> continue
handlePromptEvent _ (Prompt _ SPointOnMap _ _ _) _ = continue

handlePromptEvent
  msg
  (Prompt c SFire (FirePromptState pos) pri@(origin, range) cb)
  (VtyEvent (EvKey (KChar (directionFromChar -> Just dir)) []))
  = do
  let pos' = move dir pos
      prompt' = Prompt c SFire (FirePromptState pos') pri cb
  when (Data.distance origin pos' <= range) $
    promptState .= WaitingPrompt msg prompt'
  continue

handlePromptEvent
  _
  (Prompt Cancellable _ _ _ _)
  (VtyEvent (EvKey (KChar 'q') []))
  = clearPrompt >> continue
handlePromptEvent _ _ _ = continue

clearPrompt :: AppM ()
clearPrompt = promptState .= NoPrompt

type PromptParams :: PromptType -> Type
type family PromptParams pt where
  PromptParams ('Menu a) = Map Char (MenuOption a) -- Menu items
  PromptParams 'Fire     = Tiles -- Range
  PromptParams _         = ()

prompt
  :: forall (pt :: PromptType) (params :: Type).
    (ToJSON params, SingPromptType pt, PromptParams pt ~ ())
  => [Text]                     -- ^ Message key
  -> params                     -- ^ Message params
  -> PromptCancellable
  -> (PromptResult pt -> AppM ()) -- ^ Prompt promise handler
  -> AppM ()
prompt msgPath params cancellable cb = do
  let pt = singPromptType @pt
  msg <- Messages.message msgPath params
  mp :: Maybe (Prompt AppM) <- case pt of
    SPointOnMap -> do
      charPos <- use characterPosition
      pure . Just $ mkPointOnMapPrompt cancellable charPos cb
    SStringPrompt -> pure . Just $ mkStringPrompt cancellable cb
    SConfirm -> pure . Just $ mkPrompt cancellable pt cb
    SDirectionPrompt -> pure . Just $ mkPrompt cancellable pt cb
    SContinue -> pure . Just $ mkPrompt cancellable pt cb
  for_ mp $ \p -> promptState .= WaitingPrompt msg p

prompt_
  :: forall (pt :: PromptType).
    (SingPromptType pt, PromptParams pt ~ ())
  => [Text] -- ^ Message key
  -> PromptCancellable
  -> (PromptResult pt -> AppM ()) -- ^ Prompt promise handler
  -> AppM ()
prompt_ msg = prompt msg $ object []

stringPromptWithDefault
  :: forall (params :: Type). (ToJSON params)
  => [Text]                                -- ^ Message key
  -> params                                -- ^ Message params
  -> PromptCancellable
  -> Text                                  -- ^ Prompt default
  -> (PromptResult 'StringPrompt -> AppM ()) -- ^ Prompt promise handler
  -> AppM ()
stringPromptWithDefault msgPath params cancellable def cb = do
  msg <- Messages.message msgPath params
  let p = mkStringPromptWithDefault cancellable def cb
  promptState .= WaitingPrompt msg p

stringPromptWithDefault_
  :: [Text]                                -- ^ Message key
  -> PromptCancellable
  -> Text                                  -- ^ Prompt default
  -> (PromptResult 'StringPrompt -> AppM ()) -- ^ Prompt promise handler
  -> AppM ()
stringPromptWithDefault_ msg = stringPromptWithDefault msg $ object []

confirm
  :: ToJSON params
  => [Text] -- ^ Message key
  -> params
  -> AppM ()
  -> AppM ()
confirm msgPath params
  = prompt @'Confirm msgPath params Cancellable . const

confirm_ :: [Text] -> AppM () -> AppM ()
confirm_ msgPath = confirm msgPath $ object []

menu :: forall (a :: Type) (params :: Type).
       (ToJSON params)
     => [Text]                            -- ^ Message key
     -> params                            -- ^ Message params
     -> PromptCancellable
     -> Map Char (MenuOption a)           -- ^ Menu items
     -> (PromptResult ('Menu a) -> AppM ()) -- ^ Menu promise handler
     -> AppM ()
menu msgPath params cancellable items' cb = do
  msg <- Messages.message msgPath params
  let p = mkMenu cancellable items' cb
  promptState .= WaitingPrompt msg p

menu_ :: forall (a :: Type).
        [Text]                            -- ^ Message key
      -> PromptCancellable
      -> Map Char (MenuOption a)           -- ^ Menu items
      -> (PromptResult ('Menu a) -> AppM ()) -- ^ Menu promise handler
      -> AppM ()
menu_ msgPath = menu msgPath $ object []

firePrompt_
  :: [Text]                        -- ^ Message key
  -> PromptCancellable
  -> Tiles                         -- ^ Range
  -> (PromptResult 'Fire -> AppM ()) -- ^ Promise handler
  -> AppM ()
firePrompt_ msgPath cancellable range cb = do
  msg <- Messages.message msgPath $ object []
  initialPos <- maybe (use characterPosition) pure =<< nearestEnemyPosition
  let p = mkFirePrompt cancellable initialPos range cb
  promptState .= WaitingPrompt msg p

-- | Returns the position of the nearest visible hostile creature, if any
nearestEnemyPosition :: AppM (Maybe Position)
nearestEnemyPosition = do
  charPos <- use characterPosition
  em <- use entities
  ps <- characterVisiblePositions
  let candidates = toList ps >>= \p ->
        let ents = EntityMap.atPositionWithIDs p em
        in ents
           ^.. folded
           . _2
           . positioned
           . _SomeEntity @Creature
           . creatureType
           . filtered (view hostile)
           . to (const (distance charPos p, p))
  pure . headMay . fmap snd $ sortOn fst candidates
  where
    distance :: Position -> Position -> Double
    distance = Metric.distance `on` (fmap fromIntegral . view _Position)
