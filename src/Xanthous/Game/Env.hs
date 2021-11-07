{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Xanthous.Game.Env
  ( Config(..)
  , defaultConfig
  , disableSaving
  , GameEnv(..)
  , eventChan
  , config
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import Brick.BChan (BChan)
import Xanthous.Data.App (AppEvent)
--------------------------------------------------------------------------------

data Config = Config
  { _disableSaving :: Bool
  }
  deriving stock (Generic, Show, Eq)
makeLenses ''Config
{-# ANN Config ("HLint: ignore Use newtype instead of data" :: String) #-}

defaultConfig :: Config
defaultConfig = Config
  { _disableSaving = False
  }

--------------------------------------------------------------------------------

data GameEnv = GameEnv
  { _eventChan :: BChan AppEvent
  , _config :: Config
  }
  deriving stock (Generic)
makeLenses ''GameEnv
