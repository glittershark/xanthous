{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Xanthous.Data.Levels
  ( Levels
  , allLevels
  , nextLevel
  , prevLevel
  , mkLevels1
  , mkLevels
  , oneLevel
  , current
  , ComonadStore(..)
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding ((<.>), Empty, foldMap)
import           Xanthous.Util (between, EqProp, EqEqProp(..))
import           Xanthous.Util.Comonad (current)
import           Xanthous.Orphans ()
--------------------------------------------------------------------------------
import           Control.Comonad.Store
import           Control.Comonad.Store.Zipper
import           Data.Aeson (ToJSON(..), FromJSON(..))
import           Data.Aeson.Generic.DerivingVia
import           Data.Functor.Apply
import           Data.Foldable (foldMap)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromJust)
import           Data.Sequence (Seq((:<|), Empty))
import           Data.Semigroup.Foldable.Class
import           Data.Text (replace)
import           Test.QuickCheck
--------------------------------------------------------------------------------

-- | Collection of levels plus a pointer to the current level
--
-- Navigation is via the 'Comonad' instance. We can get the current level with
-- 'extract':
--
--     extract @Levels :: Levels level -> level
--
-- For access to and modification of the level, use
-- 'Xanthous.Util.Comonad.current'
newtype Levels a = Levels { levelZipper :: Zipper Seq a }
    deriving stock (Generic)
    deriving (Functor, Comonad, Foldable) via (Zipper Seq)
    deriving (ComonadStore Int) via (Zipper Seq)

type instance Element (Levels a) = a
instance MonoFoldable (Levels a)
instance MonoFunctor (Levels a)
instance MonoTraversable (Levels a)

instance Traversable Levels where
  traverse f (Levels z) = Levels <$> traverse f z

instance Foldable1 Levels

instance Traversable1 Levels where
  traverse1 f (Levels z) = seek (pos z) . partialMkLevels <$> go (unzipper z)
    where
      go Empty = error "empty seq, unreachable"
      go (x :<| xs) = (<|) <$> f x <.> go xs

-- | Always takes the position of the latter element
instance Semigroup (Levels a) where
  levs₁ <> levs₂
    = seek (pos levs₂)
    . partialMkLevels
    $ allLevels levs₁ <> allLevels levs₂

-- | Make Levels from a Seq. Throws an error if the seq is not empty
partialMkLevels :: Seq a -> Levels a
partialMkLevels = Levels . fromJust . zipper

-- | Make Levels from a possibly-empty structure
mkLevels :: Foldable1 f => f level -> Maybe (Levels level)
mkLevels = fmap Levels . zipper . foldMap pure

-- | Make Levels from a non-empty structure
mkLevels1 :: Foldable1 f => f level -> Levels level
mkLevels1 = fromJust . mkLevels

oneLevel :: a -> Levels a
oneLevel = mkLevels1 . Identity

-- | Get a sequence of all the levels
allLevels :: Levels a -> Seq a
allLevels = unzipper . levelZipper

-- | Step to the next level, generating a new level if necessary using the given
-- applicative action
nextLevel
  :: Applicative m
  => m level -- ^ Generate a new level, if necessary
  -> Levels level
  -> m (Levels level)
nextLevel genLevel levs
  | pos levs + 1 < size (levelZipper levs)
  = pure $ seeks succ levs
  | otherwise
  = genLevel <&> \level ->
      seek (pos levs + 1) . partialMkLevels $ allLevels levs |> level

-- | Go to the previous level. Returns Nothing if 'pos' is 0
prevLevel :: Levels level -> Maybe (Levels level)
prevLevel levs | pos levs == 0 = Nothing
               | otherwise = Just $ seeks pred levs

--------------------------------------------------------------------------------

-- | alternate, slower representation of Levels we can Iso into to perform
-- various operations
data AltLevels a = AltLevels
  { _levels :: NonEmpty a
  , _currentLevel :: Int -- ^ invariant: is within the bounds of _levels
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ FieldLabelModifier '[Drop 1] ]
           (AltLevels a)
makeLenses ''AltLevels

alt :: Iso (Levels a) (Levels b) (AltLevels a) (AltLevels b)
alt = iso hither yon
  where
    hither levs = AltLevels (NE.fromList . toList $ allLevels levs) (pos levs)
    yon (AltLevels levs curr) = seek curr $ mkLevels1 levs

instance Eq a => Eq (Levels a) where
  (==) = (==) `on` view alt

deriving via EqEqProp (Levels a) instance Eq a => EqProp (Levels a)

instance Show a => Show (Levels a) where
  show = unpack . replace "AltLevels" "Levels" . pack . show . view alt

instance NFData a => NFData (Levels a) where
  rnf = rnf . view alt

instance ToJSON a => ToJSON (Levels a) where
  toJSON = toJSON . view alt

instance FromJSON a => FromJSON (Levels a) where
  parseJSON = fmap (review alt) . parseJSON

instance Arbitrary a => Arbitrary (AltLevels a) where
  arbitrary = do
    _levels <- arbitrary
    _currentLevel <- choose (0, length _levels - 1)
    pure AltLevels {..}
  shrink als = do
    _levels <- shrink $ als ^. levels
    _currentLevel <- filter (between 0 $ length _levels - 1)
                    $ shrink $ als ^. currentLevel
    pure AltLevels {..}


instance Arbitrary a => Arbitrary (Levels a) where
  arbitrary = review alt <$> arbitrary
  shrink = fmap (review alt) . shrink . view alt

instance CoArbitrary a => CoArbitrary (Levels a) where
  coarbitrary = coarbitrary . view alt

instance Function a => Function (Levels a) where
  function = functionMap (view alt) (review alt)
