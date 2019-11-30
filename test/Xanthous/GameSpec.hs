module Xanthous.GameSpec where

import Test.Prelude hiding (Down)
import Xanthous.Game
import Xanthous.Game.State
import Control.Lens.Properties
import Xanthous.Data (move, Direction(Down))
import Xanthous.Data.EntityMap (atPosition)

main :: IO ()
main = defaultMain test

test :: TestTree
test
  = localOption (QuickCheckTests 10)
  . localOption (QuickCheckMaxSize 10)
  $ testGroup "Xanthous.Game"
  [ testGroup "positionedCharacter"
    [ testProperty "lens laws" $ isLens positionedCharacter
    , testCase "updates the position of the character" $ do
      initialGame <- getInitialState
      let initialPos = initialGame ^. characterPosition
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
  , testGroup "character"
    [ testProperty "lens laws" $ isLens character
    ]
  , testGroup "MessageHistory"
    [ testGroup "MonoComonad laws"
      [ testProperty "oextend oextract ≡ id"
        $ \(mh :: MessageHistory) -> oextend oextract mh === mh
      , testProperty "oextract ∘ oextend f ≡ f"
        $ \(mh :: MessageHistory) f -> (oextract . oextend f) mh === f mh
      , testProperty "oextend f ∘ oextend g ≡ oextend (f . oextend g)"
        $ \(mh :: MessageHistory) f g ->
          (oextend f . oextend g) mh === oextend (f . oextend g) mh
      ]
    ]
  , testGroup "Saving the game"
    [ testProperty "forms a prism" $ isPrism saved
    , testProperty "round-trips" $ \gs ->
        loadGame (saveGame gs) === Just gs
    , testProperty "preserves the character ID" $ \gs ->
        let Just gs' = loadGame $ saveGame gs
        in gs' ^. character === gs ^. character
    ]
  ]
