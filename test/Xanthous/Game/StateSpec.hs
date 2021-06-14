--------------------------------------------------------------------------------
module Xanthous.Game.StateSpec (main, test) where
--------------------------------------------------------------------------------
import           Test.Prelude
--------------------------------------------------------------------------------
import           Xanthous.Game.State
import           Xanthous.Entities.Raws (raws, entityFromRaw)
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Game.StateSpec"
  [ testGroup "entityTypeName"
    [ testCase "for a creature" $
        let gormlakRaw = raws ^?! ix "gormlak"
            creature = entityFromRaw gormlakRaw
        in entityTypeName creature @?= "Creature"
    , testCase "for an item" $
        let stickRaw = raws ^?! ix "stick"
            item = entityFromRaw stickRaw
        in entityTypeName item @?= "Item"
    ]
  ]
