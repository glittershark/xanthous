module Xanthous.Game
  ( GameState(..)
  , levels
  , entities
  , revealedPositions
  , messageHistory
  , randomGen
  , promptState
  , GamePromptState(..)

  , getInitialState
  , initialStateFromSeed

  , positionedCharacter
  , character
  , characterPosition
  , updateCharacterVision
  , characterVisiblePositions
  , entitiesAtCharacter
  , revealedEntitiesAtPosition

    -- * Messages
  , MessageHistory(..)
  , HasMessages(..)
  , HasTurn(..)
  , HasDisplayedTurn(..)
  , pushMessage
  , previousMessage
  , nextTurn

    -- * Collisions
  , Collision(..)
  , collisionAt

    -- * App monad
  , AppT(..)

    -- * Saving the game
  , saveGame
  , loadGame
  , saved

    -- * Debug State
  , DebugState(..)
  , debugState
  , allRevealed
  ) where
--------------------------------------------------------------------------------
import qualified Codec.Compression.Zlib as Zlib
import           Codec.Compression.Zlib.Internal (DecompressError)
import qualified Data.Aeson as JSON
import           System.IO.Unsafe
--------------------------------------------------------------------------------
import           Xanthous.Prelude
import           Xanthous.Game.State
import           Xanthous.Game.Lenses
import           Xanthous.Game.Arbitrary ()
import           Xanthous.Entities.Entities ()
--------------------------------------------------------------------------------

saveGame :: GameState -> LByteString
saveGame = Zlib.compress . JSON.encode

loadGame :: LByteString -> Maybe GameState
loadGame = JSON.decode <=< decompressZlibMay
  where
    decompressZlibMay bs
      = unsafeDupablePerformIO
      $ (let r = Zlib.decompress bs in r `seq` pure (Just r))
      `catch` \(_ :: DecompressError) -> pure Nothing

saved :: Prism' LByteString GameState
saved = prism' saveGame loadGame
