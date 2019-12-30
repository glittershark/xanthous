--------------------------------------------------------------------------------
module Xanthous.Data.EntityMap.GraphicsSpec (main, test) where
--------------------------------------------------------------------------------
import Test.Prelude
import Data.Aeson
--------------------------------------------------------------------------------
import Xanthous.Game.State
import Xanthous.Data
import Xanthous.Data.EntityMap
import Xanthous.Data.EntityMap.Graphics
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Data.EntityMap.Graphics"
  [ testGroup "visiblePositions"
    [ testCase "non-contiguous bug 1" $
        let charPos = Position 20 20
            gormlakPos = Position 17 19
            em = insertAt gormlakPos TestEntity
               . insertAt charPos TestEntity
               $ mempty
            visPositions = visiblePositions charPos 12 em
        in (gormlakPos `member` visPositions) @?
          ( "not ("
          <> show gormlakPos <> " `member` "
          <> show visPositions
          <> ")"
          )
    ]
  ]

--------------------------------------------------------------------------------

data TestEntity = TestEntity
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

instance Brain TestEntity where
  step _ = pure
instance Draw TestEntity
instance Entity TestEntity where
  blocksVision _ = False
  description _ = ""
  entityChar _ = "e"