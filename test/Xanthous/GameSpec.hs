module Xanthous.GameSpec where

import Test.Prelude hiding (Down)
import Xanthous.Game
import Control.Lens.Properties
import Xanthous.Data (move, Direction(Down))
import Xanthous.Data.EntityMap (atPosition)
import Xanthous.Entities.SomeEntity

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Game"
  [ testGroup "positionedCharacter"
    [ testProperty "lens laws" $ isLens positionedCharacter
    , testCase "updates the position of the character" $ do
      let initialGame = getInitialState
          initialPos = initialGame ^. characterPosition
          updatedGame = initialGame & characterPosition %~ move Down
          updatedPos = updatedGame ^. characterPosition
      updatedPos @?= move Down initialPos
      updatedGame ^. entities . atPosition initialPos @?= fromList []
      updatedGame ^. entities . atPosition updatedPos
        @?= fromList [SomeEntity $ initialGame ^. character]
    ]
  , testGroup "characterPosition"
    [ testProperty "lens laws" $ isLens characterPosition
    ]
  ]
