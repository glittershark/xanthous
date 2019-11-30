{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Xanthous.Game.Arbitrary where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Test.QuickCheck
import           System.Random
--------------------------------------------------------------------------------
import           Xanthous.Game.State
import           Xanthous.Entities.Entities ()
import           Xanthous.Entities.Character
import qualified Xanthous.Data.EntityMap as EntityMap
--------------------------------------------------------------------------------

instance Arbitrary GameState where
  arbitrary = do
    chr <- arbitrary @Character
    charPos <- arbitrary
    _messageHistory <- arbitrary
    (_characterEntityID, _entities) <- arbitrary <&>
      EntityMap.insertAtReturningID charPos (SomeEntity chr)
    _revealedPositions <- fmap setFromList . sublistOf $ EntityMap.positions _entities
    _randomGen <- mkStdGen <$> arbitrary
    let _promptState = NoPrompt -- TODO
    _debugState <- arbitrary
    pure $ GameState {..}


instance CoArbitrary GameState
instance Function GameState
deriving newtype instance CoArbitrary (m (a, GameState)) => CoArbitrary (AppT m a)
