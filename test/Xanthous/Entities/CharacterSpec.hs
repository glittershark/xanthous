{-# OPTIONS_GHC -Wno-type-defaults #-}
--------------------------------------------------------------------------------
module Xanthous.Entities.CharacterSpec (main, test) where
--------------------------------------------------------------------------------
import           Test.Prelude
import           Data.Vector.Lens (toVectorOf)
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
  , testGroup "Inventory"
    [ testProperty "items === itemsWithPosition . _2" $ \inv ->
        inv ^.. items === inv ^.. itemsWithPosition . _2
    , testGroup "removeItemFromPosition" $
      let rewield w inv =
            let (old, inv') = inv & wielded <<.~ w
            in inv' & backpack <>~ toVectorOf (wieldedItems . wieldedItem) old
      in [ (Backpack, \item -> backpack %~ (item ^. wieldedItem <|))
         , (LeftHand, rewield . inLeftHand)
         , (RightHand, rewield . inRightHand)
         , (BothHands, rewield . review doubleHanded)
         ] <&> \(pos, addItem) ->
           testProperty (show pos) $ \inv item ->
             let inv' = addItem item inv
                 inv'' = removeItemFromPosition pos (item ^. wieldedItem) inv'
             in inv'' ^.. items === inv ^.. items
    ]
  ]
