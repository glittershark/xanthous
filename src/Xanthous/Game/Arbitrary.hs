{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Xanthous.Game.Arbitrary where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (foldMap)
--------------------------------------------------------------------------------
import           Test.QuickCheck
import           System.Random
import           Data.Foldable (foldMap)
--------------------------------------------------------------------------------
import           Xanthous.Data.Levels
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Entities.Entities ()
import           Xanthous.Entities.Character
import           Xanthous.Game.State
import           Xanthous.Orphans ()
import           Xanthous.Util.QuickCheck (GenericArbitrary(..))
--------------------------------------------------------------------------------

deriving via GenericArbitrary GameLevel instance Arbitrary GameLevel

instance Arbitrary GameState where
  arbitrary = do
    chr <- arbitrary @Character
    _upStaircasePosition <- arbitrary
    _messageHistory <- arbitrary
    levs <- arbitrary @(Levels GameLevel)
    _levelRevealedPositions <-
      fmap setFromList
      . sublistOf
      . foldMap (EntityMap.positions . _levelEntities)
      $ levs
    let (_characterEntityID, _levelEntities) =
          EntityMap.insertAtReturningID _upStaircasePosition (SomeEntity chr)
          $ levs ^. current . levelEntities
        _levels = levs & current .~ GameLevel {..}
    _randomGen <- mkStdGen <$> arbitrary
    let _promptState = NoPrompt -- TODO
    _activePanel <- arbitrary
    _debugState <- arbitrary
    let _autocommand = NoAutocommand
    _memo <- arbitrary
    pure $ GameState {..}


instance CoArbitrary GameLevel
instance Function GameLevel
instance CoArbitrary GameState
instance Function GameState
