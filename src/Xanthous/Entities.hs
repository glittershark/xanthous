{-# LANGUAGE UndecidableInstances #-}

module Xanthous.Entities
  ( Draw(..)
  , DrawCharacter(..)
  , DrawStyledCharacter(..)
  , Entity

  , Color(..)
  , KnownColor(..)
  ) where

import Xanthous.Prelude
import Brick
import Data.Typeable
import qualified Graphics.Vty.Attributes as Vty
import qualified Graphics.Vty.Image as Vty

class Draw a where
  draw :: a -> Widget n

newtype DrawCharacter (char :: Symbol) (a :: Type) where
  DrawCharacter :: a -> DrawCharacter char a

instance KnownSymbol char => Draw (DrawCharacter char a) where
  draw _ = str $ symbolVal @char Proxy

--------------------------------------------------------------------------------

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

class KnownColor (color :: Color) where
  colorVal :: forall proxy. proxy color -> Vty.Color

instance KnownColor 'Black where colorVal _ = Vty.black
instance KnownColor 'Red where colorVal _ = Vty.red
instance KnownColor 'Green where colorVal _ = Vty.green
instance KnownColor 'Yellow where colorVal _ = Vty.yellow
instance KnownColor 'Blue where colorVal _ = Vty.blue
instance KnownColor 'Magenta where colorVal _ = Vty.magenta
instance KnownColor 'Cyan where colorVal _ = Vty.cyan
instance KnownColor 'White where colorVal _ = Vty.white

newtype DrawStyledCharacter (fg :: Color) (bg :: Color) (char :: Symbol) (a :: Type) where
  DrawStyledCharacter :: a -> DrawStyledCharacter fg bg char a

instance
  ( KnownColor fg
  , KnownColor bg
  , KnownSymbol char
  )
  => Draw (DrawStyledCharacter fg bg char a) where
  draw _ = raw $ Vty.string attr $ symbolVal @char Proxy
    where attr = Vty.Attr
            { Vty.attrStyle = Vty.Default
            , Vty.attrForeColor = Vty.SetTo $ colorVal @fg Proxy
            , Vty.attrBackColor = Vty.SetTo $ colorVal @bg Proxy
            , Vty.attrURL = Vty.Default
            }

--------------------------------------------------------------------------------

class (Show a, Eq a, Draw a) => Entity a
instance (Show a, Eq a, Draw a) => Entity a
