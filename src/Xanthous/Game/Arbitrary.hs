{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Xanthous.Game.Arbitrary where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (levels, foldMap)
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
--------------------------------------------------------------------------------

instance Arbitrary GameState where
  arbitrary = do
    chr <- arbitrary @Character
    charPos <- arbitrary
    _messageHistory <- arbitrary
    levels <- arbitrary
    let (_characterEntityID, currentLevel) =
          EntityMap.insertAtReturningID charPos (SomeEntity chr)
          $ extract levels
        _levels = levels & current .~ currentLevel
    _revealedPositions <- fmap setFromList . sublistOf
                         $ foldMap EntityMap.positions levels
    _randomGen <- mkStdGen <$> arbitrary
    let _promptState = NoPrompt -- TODO
    _activePanel <- arbitrary
    _debugState <- arbitrary
    _sentWelcome <- arbitrary
    pure $ GameState {..}


instance CoArbitrary GameState
instance Function GameState
deriving newtype instance CoArbitrary (m (a, GameState)) => CoArbitrary (AppT m a)
