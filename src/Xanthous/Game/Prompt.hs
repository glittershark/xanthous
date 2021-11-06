{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE GADTs                #-}
--------------------------------------------------------------------------------
module Xanthous.Game.Prompt
  ( PromptType(..)
  , SPromptType(..)
  , SingPromptType(..)
  , PromptCancellable(..)
  , PromptResult(..)
  , PromptState(..)
  , promptStatePosition
  , MenuOption(..)
  , mkMenuItems
  , PromptInput
  , Prompt(..)
  , mkPrompt
  , mkStringPrompt
  , mkStringPromptWithDefault
  , mkMenu
  , mkPointOnMapPrompt
  , mkFirePrompt
  , isCancellable
  , submitPrompt
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Brick.Widgets.Edit (Editor, editorText, getEditContents)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic
--------------------------------------------------------------------------------
import           Xanthous.Util (smallestNotIn, AlphaChar (..))
import           Xanthous.Data (Direction, Position, Tiles)
import           Xanthous.Data.App (ResourceName)
import qualified Xanthous.Data.App as Resource
--------------------------------------------------------------------------------

data PromptType where
  StringPrompt    :: PromptType
  Confirm         :: PromptType
  Menu            :: Type -> PromptType
  DirectionPrompt :: PromptType
  PointOnMap      :: PromptType
  -- | Throw an item or fire a projectile weapon. Prompt is to select the
  -- direction
  Fire            :: PromptType
  Continue        :: PromptType
  deriving stock (Generic)

instance Show PromptType where
  show StringPrompt = "StringPrompt"
  show Confirm = "Confirm"
  show (Menu _) = "Menu"
  show DirectionPrompt = "DirectionPrompt"
  show PointOnMap = "PointOnMap"
  show Continue = "Continue"
  show Fire = "Fire"

data SPromptType :: PromptType -> Type where
  SStringPrompt    :: SPromptType 'StringPrompt
  SConfirm         :: SPromptType 'Confirm
  SMenu            :: SPromptType ('Menu a)
  SDirectionPrompt :: SPromptType 'DirectionPrompt
  SPointOnMap      :: SPromptType 'PointOnMap
  SContinue        :: SPromptType 'Continue
  SFire            :: SPromptType 'Fire

instance NFData (SPromptType pt) where
  rnf SStringPrompt = ()
  rnf SConfirm = ()
  rnf SMenu = ()
  rnf SDirectionPrompt = ()
  rnf SPointOnMap = ()
  rnf SContinue = ()
  rnf SFire = ()

class SingPromptType pt where singPromptType :: SPromptType pt
instance SingPromptType 'StringPrompt where singPromptType = SStringPrompt
instance SingPromptType 'Confirm where singPromptType = SConfirm
instance SingPromptType 'DirectionPrompt where singPromptType = SDirectionPrompt
instance SingPromptType 'PointOnMap where singPromptType = SPointOnMap
instance SingPromptType 'Continue where singPromptType = SContinue
instance SingPromptType 'Fire where singPromptType = SFire

instance Show (SPromptType pt) where
  show SStringPrompt    = "SStringPrompt"
  show SConfirm         = "SConfirm"
  show SMenu            = "SMenu"
  show SDirectionPrompt = "SDirectionPrompt"
  show SPointOnMap      = "SPointOnMap"
  show SContinue        = "SContinue"
  show SFire            = "SFire"

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
  FireResult       :: Position  -> PromptResult 'Fire
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

instance Arbitrary (PromptResult 'Fire) where
  arbitrary = FireResult <$> arbitrary

--------------------------------------------------------------------------------

data PromptState pt where
  StringPromptState
    :: Editor Text ResourceName     -> PromptState 'StringPrompt
  DirectionPromptState  ::            PromptState 'DirectionPrompt
  ContinuePromptState   ::            PromptState 'Continue
  ConfirmPromptState    ::            PromptState 'Confirm
  MenuPromptState       :: forall a.       PromptState ('Menu a)
  PointOnMapPromptState :: Position -> PromptState 'PointOnMap
  FirePromptState       :: Position -> PromptState 'Fire

instance NFData (PromptState pt) where
  rnf sps@(StringPromptState ed) = sps `deepseq` ed `deepseq` ()
  rnf DirectionPromptState = ()
  rnf ContinuePromptState = ()
  rnf ConfirmPromptState = ()
  rnf MenuPromptState = ()
  rnf pomps@(PointOnMapPromptState pos) = pomps `deepseq` pos `deepseq` ()
  rnf fps@(FirePromptState pos) = fps `deepseq` pos `deepseq` ()

instance Arbitrary (PromptState 'StringPrompt) where
  arbitrary = StringPromptState <$> arbitrary

instance Arbitrary (PromptState 'DirectionPrompt) where
  arbitrary = pure DirectionPromptState

instance Arbitrary (PromptState 'Continue) where
  arbitrary = pure ContinuePromptState

instance Arbitrary (PromptState ('Menu a)) where
  arbitrary = pure MenuPromptState

instance Arbitrary (PromptState 'Fire) where
  arbitrary = FirePromptState <$> arbitrary

instance CoArbitrary (PromptState 'StringPrompt) where
  coarbitrary (StringPromptState ed) = coarbitrary ed

instance CoArbitrary (PromptState 'DirectionPrompt) where
  coarbitrary DirectionPromptState = coarbitrary ()

instance CoArbitrary (PromptState 'Continue) where
  coarbitrary ContinuePromptState = coarbitrary ()

instance CoArbitrary (PromptState ('Menu a)) where
  coarbitrary MenuPromptState = coarbitrary ()

instance CoArbitrary (PromptState 'Fire) where
  coarbitrary (FirePromptState pos) = coarbitrary pos

deriving stock instance Show (PromptState pt)

-- | Traversal over the position for the prompt types with positions in their
-- prompt state (currently 'Fire' and 'PointOnMap')
promptStatePosition :: forall pt. Traversal' (PromptState pt) Position
promptStatePosition _ ps@(StringPromptState _) = pure ps
promptStatePosition _ DirectionPromptState = pure DirectionPromptState
promptStatePosition _ ContinuePromptState = pure ContinuePromptState
promptStatePosition _ ConfirmPromptState = pure ConfirmPromptState
promptStatePosition _ MenuPromptState = pure MenuPromptState
promptStatePosition f (PointOnMapPromptState p) = PointOnMapPromptState <$> f p
promptStatePosition f (FirePromptState p) = FirePromptState <$> f p

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
  PromptInput ('Menu a)     = Map Char (MenuOption a)
  PromptInput 'PointOnMap   = Position -- Character pos
  PromptInput 'Fire         = (Position, Tiles) -- Nearest enemy, range
  PromptInput 'StringPrompt = Maybe Text -- Default value
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
  coarbitrary (Prompt c SFire ps pri cb) =
    variant @Int 7 . coarbitrary (c, ps, pri, cb)

-- instance Function (Prompt m) where
--   function = functionMap toTuple _fromTuple
--     where
--       toTuple (Prompt c pt ps pri cb) = (c, pt, ps, pri, cb)


mkPrompt
  :: (PromptInput pt ~ ())
  => PromptCancellable       -- ^ Is the prompt cancellable or not?
  -> SPromptType pt          -- ^ The type of the prompt
  -> (PromptResult pt -> m ()) -- ^ Function to call when the prompt is complete
  -> Prompt m
mkPrompt c pt@SDirectionPrompt cb = Prompt c pt DirectionPromptState () cb
mkPrompt c pt@SContinue cb = Prompt c pt ContinuePromptState () cb
mkPrompt c pt@SConfirm cb = Prompt c pt ConfirmPromptState () cb

mkStringPrompt
  :: PromptCancellable                  -- ^ Is the prompt cancellable or not?
  -> (PromptResult 'StringPrompt -> m ()) -- ^ Function to call when the prompt is complete
  -> Prompt m
mkStringPrompt c =
  let ps = StringPromptState $ editorText Resource.Prompt (Just 1) ""
  in Prompt c SStringPrompt ps Nothing

mkStringPromptWithDefault
  :: PromptCancellable                  -- ^ Is the prompt cancellable or not?
  -> Text                               -- ^ Default value for the prompt
  -> (PromptResult 'StringPrompt -> m ()) -- ^ Function to call when the prompt is complete
  -> Prompt m
mkStringPromptWithDefault c def =
  let ps = StringPromptState $ editorText Resource.Prompt (Just 1) ""
  in Prompt c SStringPrompt ps (Just def)

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

mkFirePrompt
  :: PromptCancellable
  -> Position -- ^ Initial position
  -> Tiles    -- ^ Range
  -> (PromptResult 'Fire -> m ())
  -> Prompt m
mkFirePrompt c pos range = Prompt c SFire (FirePromptState pos) (pos, range)

isCancellable :: Prompt m -> Bool
isCancellable (Prompt Cancellable _ _ _ _)   = True
isCancellable (Prompt Uncancellable _ _ _ _) = False

submitPrompt :: Applicative m => Prompt m -> m ()
submitPrompt (Prompt _ pt ps pri cb) =
  case (pt, ps, pri) of
    (SStringPrompt, StringPromptState edit, mDef) ->
      let inputVal = mconcat . getEditContents $ edit
          val | null inputVal, Just def <- mDef = def
              | otherwise = inputVal
      in cb $ StringResult val
    (SDirectionPrompt, DirectionPromptState, _) ->
      pure () -- Don't use submit with a direction prompt
    (SContinue, ContinuePromptState, _) ->
      cb ContinueResult
    (SMenu, MenuPromptState, _) ->
      pure () -- Don't use submit with a menu prompt
    (SPointOnMap, PointOnMapPromptState pos, _) ->
      cb $ PointOnMapResult pos
    (SConfirm, ConfirmPromptState, _) ->
      cb $ ConfirmResult True
    (SFire, FirePromptState pos, _) ->
      cb $ FireResult pos
