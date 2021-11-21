--------------------------------------------------------------------------------
module Xanthous.Entities.CommonSpec (main, test) where
--------------------------------------------------------------------------------
import           Test.Prelude
import           Data.Vector.Lens (toVectorOf)
--------------------------------------------------------------------------------
import           Xanthous.Entities.Common
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Entities.CommonSpec"
  [ testGroup "Inventory"
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
