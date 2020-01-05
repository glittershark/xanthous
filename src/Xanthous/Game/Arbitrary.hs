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
--------------------------------------------------------------------------------

instance Arbitrary GameState where
  arbitrary = do
    chr <- arbitrary @Character
    charPos <- arbitrary
    _messageHistory <- arbitrary
    levs <- arbitrary
    let (_characterEntityID, currentLevel) =
          EntityMap.insertAtReturningID charPos (SomeEntity chr)
          $ extract levs
        _levels = levs & current .~ currentLevel
    _revealedPositions <- fmap setFromList . sublistOf
                         $ foldMap EntityMap.positions levs
    _randomGen <- mkStdGen <$> arbitrary
    let _promptState = NoPrompt -- TODO
    _activePanel <- arbitrary
    _debugState <- arbitrary
    _sentWelcome <- arbitrary
    pure $ GameState {..}


instance CoArbitrary GameState
instance Function GameState
deriving newtype instance CoArbitrary (m (a, GameState)) => CoArbitrary (AppT m a)
