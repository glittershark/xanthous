module Xanthous.Util.GraphicsSpec (main, test) where
--------------------------------------------------------------------------------
import Test.Prelude hiding (head)
--------------------------------------------------------------------------------
import Xanthous.Util.Graphics
import Xanthous.Util
import Data.List (head)
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Util.Graphics"
  [ testGroup "circle"
    [ testCase "radius 12, origin 0"
      $ (sort . unique @[] @[_]) (circle @Int (0, 0) 12)
      @?= (sort . unique) (
        let quadrant =
              [ (0, 12) , (1, 12) , (2, 12) , (3, 12)
              , (4, 12) , (5, 11) , (6, 11) , (7, 10)
              , (8, 9)  , (9, 9)  , (9, 8)  , (10, 7)
              , (11, 6) , (11, 5) , (12, 4) , (12, 3)
              , (12, 2) , (12, 1) , (12, 0)
              ]
        in  quadrant
         <> (quadrant <&> _1 %~ negate)
         <> (quadrant <&> _2 %~ negate)
         <> (quadrant <&> both %~ negate)
      )
    ]

  , testGroup "line"
    [ testProperty "starts and ends at the start and end points" $ \start end ->
        let ℓ = line @Int start end
        in counterexample ("line: " <> show ℓ)
        $ length ℓ > 2 ==> (head ℓ === start) .&&. (head (reverse ℓ) === end)
    ]
  ]
