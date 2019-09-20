{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
--------------------------------------------------------------------------------
module Xanthous.Game.Prompt
  ( PromptType(..)
  , SPromptType(..)
  , SingPromptType(..)
  , PromptCancellable(..)
  , PromptResult(..)
  , PromptState(..)
  , Prompt(..)
  , mkPrompt
  , isCancellable
  , submitPrompt
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import Brick.Widgets.Edit (Editor, editorText, getEditContents)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
--------------------------------------------------------------------------------
import Xanthous.Data (Direction, Position)
import Xanthous.Resource (Name)
import qualified Xanthous.Resource as Resource
--------------------------------------------------------------------------------

data PromptType where
  StringPrompt    :: PromptType
  Confirm         :: PromptType
  Menu            :: Type -> PromptType
  DirectionPrompt :: PromptType
  PointOnMap      :: PromptType
  deriving stock (Generic)

instance Show PromptType where
  show StringPrompt = "StringPrompt"
  show Confirm = "Confirm"
  show (Menu _) = "Menu"
  show DirectionPrompt = "DirectionPrompt"
  show PointOnMap = "PointOnMap"

data SPromptType :: PromptType -> Type where
  SStringPrompt    ::      SPromptType 'StringPrompt
  SConfirm         ::      SPromptType 'Confirm
  SMenu            :: forall a. SPromptType ('Menu a)
  SDirectionPrompt ::      SPromptType 'DirectionPrompt
  SPointOnMap      ::      SPromptType 'PointOnMap

class SingPromptType pt where singPromptType :: SPromptType pt
instance SingPromptType 'StringPrompt where singPromptType = SStringPrompt

instance Show (SPromptType pt) where
  show SStringPrompt    = "SStringPrompt"
  show SConfirm         = "SConfirm"
  show SMenu            = "SMenu"
  show SDirectionPrompt = "SDirectionPrompt"
  show SPointOnMap      = "SPointOnMap"

data PromptCancellable
  = Cancellable
  | Uncancellable
  deriving stock (Show, Eq, Ord, Enum, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)

instance Arbitrary PromptCancellable where
  arbitrary = genericArbitrary

data PromptResult (pt :: PromptType) where
  StringResult     :: Text      -> PromptResult 'StringPrompt
  ConfirmResult    :: Bool      -> PromptResult 'Confirm
  MenuResult       :: forall a. a    -> PromptResult ('Menu a)
  DirectionResult  :: Direction -> PromptResult 'DirectionPrompt
  PointOnMapResult :: Position  -> PromptResult 'PointOnMap

data PromptState pt where
  StringPromptState :: Editor Text Name -> PromptState 'StringPrompt

deriving stock instance Show (PromptState pt)

data Prompt (m :: Type -> Type) where
  Prompt
    :: forall (pt :: PromptType)
        (m :: Type -> Type).
      PromptCancellable
    -> SPromptType pt
    -> PromptState pt
    -> (PromptResult pt -> m ())
    -> Prompt m

instance Show (Prompt m) where
  show (Prompt c pt ps _)
    = "(Prompt "
    <> show c <> " "
    <> show pt <> " "
    <> show ps
    <> " <function> )"

mkPrompt :: PromptCancellable -> SPromptType pt -> (PromptResult pt -> m ()) -> Prompt m
mkPrompt c pt@SStringPrompt cb =
  let ps = StringPromptState $ editorText Resource.Prompt (Just 1) ""
  in Prompt c pt ps cb
mkPrompt _ _ _ = undefined

isCancellable :: Prompt m -> Bool
isCancellable (Prompt Cancellable _ _ _)   = True
isCancellable (Prompt Uncancellable _ _ _) = False

submitPrompt :: Prompt m -> m ()
submitPrompt (Prompt _ pt ps cb) =
  case (pt, ps) of
    (SStringPrompt, StringPromptState edit) ->
      cb . StringResult . mconcat . getEditContents $ edit
    _ -> undefined

-- data PromptInput :: PromptType -> Type where
--   StringInput :: PromptInput 'StringPrompt
