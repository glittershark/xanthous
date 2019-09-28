{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import           Xanthous.Entities.Arbitrary ()
import           Xanthous.Entities.Character
import qualified Xanthous.Data.EntityMap as EntityMap
--------------------------------------------------------------------------------

instance Arbitrary GameState where
  arbitrary = do
    char <- arbitrary @Character
    charPos <- arbitrary
    _messageHistory <- arbitrary
    (_characterEntityID, _entities) <- arbitrary <&>
      EntityMap.insertAtReturningID charPos (SomeEntity char)
    _revealedPositions <- fmap setFromList . sublistOf $ EntityMap.positions _entities
    _randomGen <- mkStdGen <$> arbitrary
    let _promptState = NoPrompt -- TODO
    pure $ GameState {..}
