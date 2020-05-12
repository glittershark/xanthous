{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Xanthous.Game.Env
  ( GameEnv(..)
  , eventChan
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import Brick.BChan (BChan)
import Xanthous.Data.App (AppEvent)
--------------------------------------------------------------------------------

data GameEnv = GameEnv
  { _eventChan :: BChan AppEvent
  }
  deriving stock (Generic)
makeLenses ''GameEnv
{-# ANN GameEnv ("HLint: ignore Use newtype instead of data" :: String) #-}
