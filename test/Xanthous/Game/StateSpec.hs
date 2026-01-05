--------------------------------------------------------------------------------
module Xanthous.Game.StateSpec (main, test) where
--------------------------------------------------------------------------------
import           Test.Prelude
--------------------------------------------------------------------------------
import           Xanthous.Game.State
import           Xanthous.Entities.Raws (raws)
import           Xanthous.Generators.Level.LevelContents (entityFromRaw)
import           Xanthous.Random
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Game.StateSpec"
  [ testGroup "entityTypeName"
    [ testCase "for a creature" $ do
        let gormlakRaw = raws ^?! ix "gormlak"
        creature <- evalRandIO $ entityFromRaw gormlakRaw
        entityTypeName creature @?= "Creature"
    , testCase "for an item" $ do
        let stickRaw = raws ^?! ix "stick"
        item <- evalRandIO $ entityFromRaw stickRaw
        entityTypeName item @?= "Item"
    ]
  ]
