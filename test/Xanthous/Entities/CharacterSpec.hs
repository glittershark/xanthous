{-# OPTIONS_GHC -Wno-type-defaults #-}
--------------------------------------------------------------------------------
module Xanthous.Entities.CharacterSpec (main, test) where
--------------------------------------------------------------------------------
import           Test.Prelude
--------------------------------------------------------------------------------
import           Xanthous.Entities.Character
import           Xanthous.Util (endoTimes)
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Entities.CharacterSpec"
  [ testGroup "Knuckles"
    [ testBatch $ monoid @Knuckles mempty
    , testGroup "damageKnuckles"
      [ testCase "caps at 5" $
          let knuckles' = endoTimes 6 damageKnuckles mempty
          in _knuckleDamage knuckles' @?= 5
      ]
    ]
  ]
