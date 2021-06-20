{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
--------------------------------------------------------------------------------
module Xanthous.Game.Prompt
  ( PromptType(..)
  , SPromptType(..)
  , SingPromptType(..)
  , PromptCancellable(..)
  , PromptResult(..)
  , PromptState(..)
  , MenuOption(..)
  , mkMenuItems
  , PromptInput
  , Prompt(..)
  , mkPrompt
  , mkMenu
  , mkPointOnMapPrompt
  , isCancellable
  , submitPrompt
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import           Brick.Widgets.Edit (Editor, editorText, getEditContents)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
import           Xanthous.Util (smallestNotIn, AlphaChar (..))
import           Xanthous.Data (Direction, Position)
import           Xanthous.Data.App (ResourceName)
import qualified Xanthous.Data.App as Resource
--------------------------------------------------------------------------------

data PromptType where
  StringPrompt    :: PromptType
  Confirm         :: PromptType
  Menu            :: Type -> PromptType
  DirectionPrompt :: PromptType
  PointOnMap      :: PromptType
  Continue        :: PromptType
  deriving stock (Generic)

instance Show PromptType where
  show StringPrompt = "StringPrompt"
  show Confirm = "Confirm"
  show (Menu _) = "Menu"
  show DirectionPrompt = "DirectionPrompt"
  show PointOnMap = "PointOnMap"
  show Continue = "Continue"

data SPromptType :: PromptType -> Type where
  SStringPrompt    ::      SPromptType 'StringPrompt
  SConfirm         ::      SPromptType 'Confirm
  SMenu            ::      SPromptType ('Menu a)
  SDirectionPrompt ::      SPromptType 'DirectionPrompt
  SPointOnMap      ::      SPromptType 'PointOnMap
  SContinue        ::      SPromptType 'Continue

instance NFData (SPromptType pt) where
  rnf SStringPrompt = ()
  rnf SConfirm = ()
  rnf SMenu = ()
  rnf SDirectionPrompt = ()
  rnf SPointOnMap = ()
  rnf SContinue = ()

class SingPromptType pt where singPromptType :: SPromptType pt
instance SingPromptType 'StringPrompt where singPromptType = SStringPrompt
instance SingPromptType 'Confirm where singPromptType = SConfirm
instance SingPromptType 'DirectionPrompt where singPromptType = SDirectionPrompt
instance SingPromptType 'PointOnMap where singPromptType = SPointOnMap
instance SingPromptType 'Continue where singPromptType = SContinue

instance Show (SPromptType pt) where
  show SStringPrompt    = "SStringPrompt"
  show SConfirm         = "SConfirm"
  show SMenu            = "SMenu"
  show SDirectionPrompt = "SDirectionPrompt"
  show SPointOnMap      = "SPointOnMap"
  show SContinue        = "SContinue"

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
  ContinueResult   ::             PromptResult 'Continue

instance Arbitrary (PromptResult 'StringPrompt) where
  arbitrary = StringResult <$> arbitrary

instance Arbitrary (PromptResult 'Confirm) where
  arbitrary = ConfirmResult <$> arbitrary

instance Arbitrary a => Arbitrary (PromptResult ('Menu a)) where
  arbitrary = MenuResult <$> arbitrary

instance Arbitrary (PromptResult 'DirectionPrompt) where
  arbitrary = DirectionResult <$> arbitrary

instance Arbitrary (PromptResult 'PointOnMap) where
  arbitrary = PointOnMapResult <$> arbitrary

instance Arbitrary (PromptResult 'Continue) where
  arbitrary = pure ContinueResult

--------------------------------------------------------------------------------

data PromptState pt where
  StringPromptState
    :: Editor Text ResourceName     -> PromptState 'StringPrompt
  DirectionPromptState  ::            PromptState 'DirectionPrompt
  ContinuePromptState   ::            PromptState 'Continue
  ConfirmPromptState    ::            PromptState 'Confirm
  MenuPromptState       :: forall a.       PromptState ('Menu a)
  PointOnMapPromptState :: Position -> PromptState 'PointOnMap

instance NFData (PromptState pt) where
  rnf sps@(StringPromptState ed) = sps `deepseq` ed `deepseq` ()
  rnf DirectionPromptState = ()
  rnf ContinuePromptState = ()
  rnf ConfirmPromptState = ()
  rnf MenuPromptState = ()
  rnf pomps@(PointOnMapPromptState pos) = pomps `deepseq` pos `deepseq` ()

instance Arbitrary (PromptState 'StringPrompt) where
  arbitrary = StringPromptState <$> arbitrary

instance Arbitrary (PromptState 'DirectionPrompt) where
  arbitrary = pure DirectionPromptState

instance Arbitrary (PromptState 'Continue) where
  arbitrary = pure ContinuePromptState

instance Arbitrary (PromptState ('Menu a)) where
  arbitrary = pure MenuPromptState

instance CoArbitrary (PromptState 'StringPrompt) where
  coarbitrary (StringPromptState ed) = coarbitrary ed

instance CoArbitrary (PromptState 'DirectionPrompt) where
  coarbitrary DirectionPromptState = coarbitrary ()

instance CoArbitrary (PromptState 'Continue) where
  coarbitrary ContinuePromptState = coarbitrary ()

instance CoArbitrary (PromptState ('Menu a)) where
  coarbitrary MenuPromptState = coarbitrary ()

deriving stock instance Show (PromptState pt)

data MenuOption a = MenuOption Text a
  deriving stock (Eq, Generic, Functor)
  deriving anyclass (NFData, CoArbitrary, Function)

instance Comonad MenuOption where
  extract (MenuOption _ x) = x
  extend cok mo@(MenuOption text _) = MenuOption text (cok mo)

mkMenuItems :: (MonoFoldable f, Element f ~ (Char, MenuOption a))
            => f
            -> Map Char (MenuOption a)
mkMenuItems = flip foldl' mempty $ \items (chr, option) ->
  let chr' = if has (ix chr) items
             then getAlphaChar . smallestNotIn . map AlphaChar $ keys items
             else chr
  in items & at chr' ?~ option

instance Show (MenuOption a) where
  show (MenuOption m _) = show m

type family PromptInput (pt :: PromptType) :: Type where
  PromptInput ('Menu a) = Map Char (MenuOption a)
  PromptInput 'PointOnMap = Position -- Character pos
  PromptInput _ = ()

data Prompt (m :: Type -> Type) where
  Prompt
    :: forall (pt :: PromptType)
        (m :: Type -> Type).
      PromptCancellable
    -> SPromptType pt
    -> PromptState pt
    -> PromptInput pt
    -> (PromptResult pt -> m ())
    -> Prompt m

instance Show (Prompt m) where
  show (Prompt c pt ps pri _)
    = "(Prompt "
    <> show c <> " "
    <> show pt <> " "
    <> show ps <> " "
    <> showPri
    <> " <function>)"
    where showPri = case pt of
            SMenu -> show pri
            _ -> "()"

instance NFData (Prompt m) where
  rnf (Prompt c SMenu ps pri cb)
            = c
    `deepseq` ps
    `deepseq` pri
    `seq` cb
    `seq` ()
  rnf (Prompt c spt ps pri cb)
            = c
    `deepseq` spt
    `deepseq` ps
    `deepseq` pri
    `seq` cb
    `seq` ()

instance CoArbitrary (m ()) => CoArbitrary (Prompt m) where
  coarbitrary (Prompt c SStringPrompt ps pri cb) =
    variant @Int 1 . coarbitrary (c, ps, pri, cb)
  coarbitrary (Prompt c SConfirm _ pri cb) = -- TODO fill in prompt state
    variant @Int 2 . coarbitrary (c, pri, cb)
  coarbitrary (Prompt c SMenu _ps _pri _cb) =
    variant @Int 3 . coarbitrary c {-, ps, pri, cb -}
  coarbitrary (Prompt c SDirectionPrompt ps pri cb) =
    variant @Int 4 . coarbitrary (c, ps, pri, cb)
  coarbitrary (Prompt c SPointOnMap _ pri cb) = -- TODO fill in prompt state
    variant @Int 5 . coarbitrary (c, pri, cb)
  coarbitrary (Prompt c SContinue ps pri cb) =
    variant @Int 6 . coarbitrary (c, ps, pri, cb)

-- instance Function (Prompt m) where
--   function = functionMap toTuple _fromTuple
--     where
--       toTuple (Prompt c pt ps pri cb) = (c, pt, ps, pri, cb)


mkPrompt :: (PromptInput pt ~ ()) => PromptCancellable -> SPromptType pt -> (PromptResult pt -> m ()) -> Prompt m
mkPrompt c pt@SStringPrompt cb =
  let ps = StringPromptState $ editorText Resource.Prompt (Just 1) ""
  in Prompt c pt ps () cb
mkPrompt c pt@SDirectionPrompt cb = Prompt c pt DirectionPromptState () cb
mkPrompt c pt@SContinue cb = Prompt c pt ContinuePromptState () cb
mkPrompt c pt@SConfirm cb = Prompt c pt ConfirmPromptState () cb

mkMenu
  :: forall a m.
    PromptCancellable
  -> Map Char (MenuOption a) -- ^ Menu items
  -> (PromptResult ('Menu a) -> m ())
  -> Prompt m
mkMenu c = Prompt c SMenu MenuPromptState

mkPointOnMapPrompt
  :: PromptCancellable
  -> Position
  -> (PromptResult 'PointOnMap -> m ())
  -> Prompt m
mkPointOnMapPrompt c pos = Prompt c SPointOnMap (PointOnMapPromptState pos) pos

isCancellable :: Prompt m -> Bool
isCancellable (Prompt Cancellable _ _ _ _)   = True
isCancellable (Prompt Uncancellable _ _ _ _) = False

submitPrompt :: Applicative m => Prompt m -> m ()
submitPrompt (Prompt _ pt ps _ cb) =
  case (pt, ps) of
    (SStringPrompt, StringPromptState edit) ->
      cb . StringResult . mconcat . getEditContents $ edit
    (SDirectionPrompt, DirectionPromptState) ->
      pure () -- Don't use submit with a direction prompt
    (SContinue, ContinuePromptState) ->
      cb ContinueResult
    (SMenu, MenuPromptState) ->
      pure () -- Don't use submit with a menu prompt
    (SPointOnMap, PointOnMapPromptState pos) ->
      cb $ PointOnMapResult pos
    (SConfirm, ConfirmPromptState) ->
      cb $ ConfirmResult True
