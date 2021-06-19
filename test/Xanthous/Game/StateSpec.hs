--------------------------------------------------------------------------------
module Xanthous.Game.StateSpec (main, test) where
--------------------------------------------------------------------------------
import           Test.Prelude
--------------------------------------------------------------------------------
import           Xanthous.Game.State
import           Xanthous.Entities.Raws (raws, entityFromRaw)
import Control.Monad.Random (evalRandT)
import System.Random (getStdGen)
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Game.StateSpec"
  [ testGroup "entityTypeName"
    [ testCase "for a creature" $ do
        let gormlakRaw = raws ^?! ix "gormlak"
        creature <- runRand $ entityFromRaw gormlakRaw
        entityTypeName creature @?= "Creature"
    , testCase "for an item" $ do
        let stickRaw = raws ^?! ix "stick"
        item <- runRand $ entityFromRaw stickRaw
        entityTypeName item @?= "Item"
    ]
  ]
  where
    runRand x = evalRandT x =<< getStdGen
