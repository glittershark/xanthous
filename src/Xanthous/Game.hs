module Xanthous.Game
  ( GameState(..)
  , entities
  , revealedPositions
  , messageHistory
  , randomGen
  , promptState
  , GamePromptState(..)

  , getInitialState

  , positionedCharacter
  , character
  , characterPosition
  , updateCharacterVision

  , MessageHistory(..)
  , pushMessage
  , popMessage
  , hideMessage

    -- * App monad
  , AppT(..)
  ) where
--------------------------------------------------------------------------------
import Xanthous.Game.State
import Xanthous.Game.Lenses
import Xanthous.Game.Arbitrary ()
