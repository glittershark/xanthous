-- |

module Xanthous.Entities.RawsSpec (main, test) where

import Test.Prelude
import Xanthous.Entities.Raws
import Xanthous.Entities.RawTypes
       (_Creature, entityName, generateParams, HasEquippedItem (equippedItem))

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Entities.Raws"
  [ testGroup "raws"
    [ testCase "are all valid" $ raws `deepseq` pure ()
    , testCase "all CreatureEquippedItems reference existent entity names" $
      let notFound
            = raws
              ^.. folded
              . _Creature
              . generateParams
              . _Just
              . equippedItem
              . _Just
              . entityName
              . filtered (isNothing . raw)
      in null notFound @? ("Some entities weren't found: " <> show notFound)
    ]
  ]
