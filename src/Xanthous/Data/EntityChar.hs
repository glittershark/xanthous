{-# LANGUAGE RoleAnnotations      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TemplateHaskell      #-}
--------------------------------------------------------------------------------
module Xanthous.Data.EntityChar
  ( EntityChar(..)
  , HasChar(..)
  , HasStyle(..)
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding ((.=))
--------------------------------------------------------------------------------
import qualified Graphics.Vty.Attributes as Vty
import           Test.QuickCheck
import           Data.Aeson
--------------------------------------------------------------------------------
import           Xanthous.Orphans ()
import           Xanthous.Util.QuickCheck (GenericArbitrary(..))
--------------------------------------------------------------------------------


class HasChar s a | s -> a where
  char :: Lens' s a
  {-# MINIMAL char #-}

data EntityChar = EntityChar
  { _char :: Char
  , _style :: Vty.Attr
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving Arbitrary via GenericArbitrary EntityChar
makeFieldsNoPrefix ''EntityChar

instance FromJSON EntityChar where
  parseJSON (String (chr :< Empty)) = pure $ EntityChar chr Vty.defAttr
  parseJSON (Object o) = do
    (EntityChar _char _) <- o .: "char"
    _style <- o .:? "style" .!= Vty.defAttr
    pure EntityChar {..}
  parseJSON _ = fail "Invalid type, expected string or object"

instance ToJSON EntityChar where
  toJSON (EntityChar chr styl)
    | styl == Vty.defAttr = String $ chr <| Empty
    | otherwise = object
      [ "char" .= chr
      , "style" .= styl
      ]

instance IsString EntityChar where
  fromString [ch] = EntityChar ch Vty.defAttr
  fromString _ = error "Entity char must only be a single character"
