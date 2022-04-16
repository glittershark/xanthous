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

newtype OneHand = OneHand Hand
  deriving stock Show

instance Arbitrary OneHand where
  arbitrary = OneHand <$> elements [LeftHand, RightHand]

otherHand :: Hand -> Hand
otherHand LeftHand = RightHand
otherHand RightHand = LeftHand
otherHand BothHands = error "OtherHand BothHands"

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
         , (InHand LeftHand, rewield . inLeftHand)
         , (InHand RightHand, rewield . inRightHand)
         , (InHand BothHands, rewield . review doubleHanded)
         ] <&> \(pos, addItem) ->
           testProperty (show pos) $ \inv item ->
             let inv' = addItem item inv
                 inv'' = removeItemFromPosition pos (item ^. wieldedItem) inv'
             in inv'' ^.. items === inv ^.. items
    ]
  , testGroup "Wielded items"
    [ testGroup "wieldInHand"
      [ testProperty "puts the item in the hand" $ \w hand item ->
          let (_, w') = wieldInHand hand item w
          in itemsInHand hand w' === [item]
      , testProperty "returns items in both hands when wielding double-handed"
        $ \lh rh newItem ->
          let w = Hands (Just lh) (Just rh)
              (prevItems, _) = wieldInHand BothHands newItem w
          in prevItems === [lh, rh]
      , testProperty "wielding in one hand leaves the item in the other hand"
        $ \(OneHand h) existingItem newItem ->
          let (_, w) = wieldInHand h existingItem nothingWielded
              (prevItems, w') = wieldInHand (otherHand h) newItem w
          in   prevItems === []
          .&&. sort (w' ^.. wieldedItems) === sort [existingItem, newItem]
      , testProperty "always leaves the same items overall" $ \w hand item ->
          let (prevItems, w') = wieldInHand hand item w
          in  sort (prevItems <> (w' ^.. wieldedItems))
          === sort (item : w ^.. wieldedItems)
      ]
    ]
  ]
