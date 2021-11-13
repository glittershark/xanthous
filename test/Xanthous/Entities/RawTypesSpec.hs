--------------------------------------------------------------------------------
module Xanthous.Entities.RawTypesSpec (main, test) where
--------------------------------------------------------------------------------
import           Test.Prelude
--------------------------------------------------------------------------------
import           Xanthous.Entities.RawTypes
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Entities.RawTypesSpec"
  [ testGroup "CreatureGenerateParams"
    [ testBatch $ monoid @CreatureGenerateParams mempty
    , testGroup "canGenerate"
      [ testProperty "no bounds" $ \level ->
          let gps = CreatureGenerateParams Nothing Nothing
          in canGenerate level gps
      , testProperty "min bound" $ \level minB ->
          let gps = CreatureGenerateParams (Just minB) Nothing
          in canGenerate level gps === (level >= minB)
      , testProperty "max bound" $ \level maxB ->
          let gps = CreatureGenerateParams Nothing (Just maxB)
          in canGenerate level gps === (level <= maxB)
      ]
    ]
  ]
