{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Xanthous.Entities.RawTypesSpec (main, test) where
--------------------------------------------------------------------------------
import           Test.Prelude
--------------------------------------------------------------------------------
import           Data.Interval (Extended(..), (<=..<=))
--------------------------------------------------------------------------------
import           Xanthous.Entities.RawTypes
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Entities.RawTypesSpec"
  [ testGroup "CreatureGenerateParams"
    [ testGroup "Ord laws"
      [ testProperty "comparability" $ \(a :: CreatureGenerateParams) b ->
          a <= b || b <= a
      , testProperty "transitivity" $ \(a :: CreatureGenerateParams) b c ->
          a <= b && b <= c ==> a <= c
      , testProperty "reflexivity" $ \(a :: CreatureGenerateParams) ->
          a <= a
      , testProperty "antisymmetry" $ \(a :: CreatureGenerateParams) b ->
          (a <= b && b <= a) == (a == b)
      ]
    , testGroup "canGenerate" $
      let makeParams minB maxB =
            let _levelRange = maybe NegInf Finite minB <=..<= maybe PosInf Finite maxB
                _equippedItem = Nothing
            in CreatureGenerateParams {..}
      in
        [ testProperty "no bounds" $ \level ->
            let gps = makeParams Nothing Nothing
            in canGenerate level gps
        , testProperty "min bound" $ \level minB ->
            let gps = makeParams (Just minB) Nothing
            in canGenerate level gps === (level >= minB)
        , testProperty "max bound" $ \level maxB ->
            let gps = makeParams Nothing (Just maxB)
            in canGenerate level gps === (level <= maxB)
        ]
    ]
  ]
