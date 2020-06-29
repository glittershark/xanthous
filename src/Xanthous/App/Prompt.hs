{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
module Xanthous.App.Prompt
  ( handlePromptEvent
  , clearPrompt
  , prompt
  , prompt_
  , confirm_
  , confirm
  , menu
  , menu_
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Brick (BrickEvent(..), Next)
import           Brick.Widgets.Edit (handleEditorEvent)
import           Data.Aeson (ToJSON, object)
import           Graphics.Vty.Input.Events (Event(EvKey), Key(..))
import           GHC.TypeLits (ErrorMessage(..))
--------------------------------------------------------------------------------
import           Xanthous.App.Common
import           Xanthous.Data (move)
import           Xanthous.Command (directionFromChar)
import           Xanthous.Data.App (ResourceName, AppEvent)
import           Xanthous.Game.Prompt
import           Xanthous.Game.State
import qualified Xanthous.Messages as Messages
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
  _
  (Prompt Cancellable _ _ _ _)
  (VtyEvent (EvKey (KChar 'q') []))
  = clearPrompt >> continue
handlePromptEvent _ _ _ = continue

clearPrompt :: AppM ()
clearPrompt = promptState .= NoPrompt

class NotMenu (pt :: PromptType)
instance NotMenu 'StringPrompt
instance NotMenu 'Confirm
instance NotMenu 'DirectionPrompt
instance NotMenu 'PointOnMap
instance NotMenu 'Continue
instance TypeError ('Text "Cannot use `prompt` or `prompt_` for menu prompts"
                    ':$$: 'Text "Use `menu` or `menu_` instead")
         => NotMenu ('Menu a)

prompt
  :: forall (pt :: PromptType) (params :: Type).
    (ToJSON params, SingPromptType pt, NotMenu pt)
  => [Text]                     -- ^ Message key
  -> params                     -- ^ Message params
  -> PromptCancellable
  -> (PromptResult pt -> AppM ()) -- ^ Prompt promise handler
  -> AppM ()
prompt msgPath params cancellable cb = do
  let pt = singPromptType @pt
  msg <- Messages.message msgPath params
  p <- case pt of
    SPointOnMap -> do
      charPos <- use characterPosition
      pure $ mkPointOnMapPrompt cancellable charPos cb
    SStringPrompt -> pure $ mkPrompt cancellable pt cb
    SConfirm -> pure $ mkPrompt cancellable pt cb
    SDirectionPrompt -> pure $ mkPrompt cancellable pt cb
    SContinue -> pure $ mkPrompt cancellable pt cb
    SMenu -> error "unreachable"
  promptState .= WaitingPrompt msg p

prompt_
  :: forall (pt :: PromptType).
    (SingPromptType pt, NotMenu pt)
  => [Text] -- ^ Message key
  -> PromptCancellable
  -> (PromptResult pt -> AppM ()) -- ^ Prompt promise handler
  -> AppM ()
prompt_ msg = prompt msg $ object []

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
