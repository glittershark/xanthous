--------------------------------------------------------------------------------
module Xanthous.Physics
  ( throwDistance
  , bluntThrowDamage
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude
import Xanthous.Data
       ( Meters
       , (:**:)(..)
       , Square
       , Grams
       , (|*|)
       , (|/|)
       , Hitpoints
       , Per (..)
       , squared
       , Uno(..), (|+|)
       )
--------------------------------------------------------------------------------

-- university shotputter can put a 16 lb shot about 14 meters
-- ≈ 7.25 kg 14 meters
-- 14m = x / (7.25kg × y + z)²
-- 14m = x / (7250g × y + z)²
--
-- we don't want to scale down too much:
--
-- 10 kg 10 meters
-- = 10000 g 10 meters
--
-- 15 kg w meters
-- = 15000 g w meters
--
-- 14m = x / (7250g × y + z)²
-- 10m = x / (10000g × y + z)²
-- wm = x / (15000g × y + z)²
--
-- w≈0.527301 ∧ y≈0.000212178 sqrt(x) ∧ z≈1.80555 sqrt(x) ∧ 22824.1 sqrt(x)!=0
--
-- x = 101500
-- y = 0.0675979
-- z = 575.231
--

-- TODO make this dynamic
strength :: Meters :**: Square Grams
strength = Times 10150000

yCoeff :: Uno Double
yCoeff = Uno 0.0675979

zCoeff :: Uno Double
zCoeff = Uno 575.231

-- | Calculate the maximum distance an object with the given weight can be
-- thrown
throwDistance
  :: Grams  -- ^ Weight of the object
  -> Meters -- ^ Max distance thrown
throwDistance weight = strength |/| squared (weight |*| yCoeff |+| zCoeff)

-- | Returns the damage dealt by a blunt object with the given weight when
-- thrown
bluntThrowDamage
  :: Grams
  -> Hitpoints
bluntThrowDamage weight = throwDamageRatio |*| weight
  where
    throwDamageRatio :: Hitpoints `Per` Grams
    throwDamageRatio = Rate $ 1 / 5000
