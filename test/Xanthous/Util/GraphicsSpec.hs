module Xanthous.Util.GraphicsSpec (main, test) where
--------------------------------------------------------------------------------
import Test.Prelude hiding (head)
--------------------------------------------------------------------------------
import Data.List (nub, head)
import Data.Set (isSubsetOf)
import Linear.V2
--------------------------------------------------------------------------------
import Xanthous.Util.Graphics
import Xanthous.Util
import Xanthous.Orphans ()
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Util.Graphics"
  [ testGroup "circle"
    [ testCase "radius 1, origin 2,2"
      {-
        |   | 0 | 1 | 2 | 3 |
        |---+---+---+---+---|
        | 0 |   |   |   |   |
        | 1 |   |   | x |   |
        | 2 |   | x |   | x |
        | 3 |   |   | x |   |
      -}
      $ (sort . unique @[] @[_]) (circle @Int (V2 2 2) 1)
      @?= [ V2 1 2
          , V2 2 1, V2 2 3
          , V2 3 2
          ]
    , testCase "radius 12, origin 0"
      $   (sort . nub) (circle @Int 0 12)
      @?= (sort . nub)
          [ V2 (-12) (-4), V2 (-12) (-3), V2 (-12) (-2), V2 (-12) (-1)
          , V2 (-12) 0, V2 (-12) 1, V2 (-12) 2, V2 (-12) 3, V2 (-12) 4
          , V2 (-11) (-6), V2 (-11) (-5), V2 (-11) 5, V2 (-11) 6, V2 (-10) (-7)
          , V2 (-10) 7, V2 (-9) (-9), V2 (-9) (-8), V2 (-9) 8, V2 (-9) 9
          , V2 (-8) (-9), V2 (-8) 9, V2 (-7) (-10), V2 (-7) 10, V2 (-6) (-11)
          , V2 (-6) 11, V2 (-5) (-11), V2 (-5) 11, V2 (-4) (-12), V2 (-4) 12
          , V2 (-3) (-12), V2 (-3) 12, V2 (-2) (-12), V2 (-2) 12, V2 (-1) (-12)
          , V2 (-1) 12, V2 0 (-12), V2 0 12, V2 1 (-12), V2 1 12, V2 2 (-12)
          , V2 2 12, V2 3 (-12), V2 3 12, V2 4 (-12), V2 4 12, V2 5 (-11)
          , V2 5 11, V2 6 (-11), V2 6 11, V2 7 (-10), V2 7 10, V2 8 (-9), V2 8 9
          , V2 9 (-9), V2 9 (-8), V2 9 8, V2 9 9, V2 10 (-7), V2 10 7
          , V2 11 (-6), V2 11 (-5), V2 11 5, V2 11 6, V2 12 (-4), V2 12 (-3)
          , V2 12 (-2), V2 12 (-1), V2 12 0, V2 12 1, V2 12 2, V2 12 3, V2 12 4
          ]
    ]
  , testGroup "filledCircle"
    [ testProperty "is a superset of circle" $ \center radius ->
        let circ = circle @Int center radius
            filledCirc = filledCircle center radius
        in counterexample ( "circle: " <> show circ
                           <> "\nfilledCircle: " <> show filledCirc)
          $ setFromList circ `isSubsetOf` setFromList filledCirc
    -- TODO later
    -- , testProperty "is always contiguous" $ \center radius ->
    --     let filledCirc = filledCircle center radius
    --     in counterexample (renderBooleanGraphics filledCirc) $
    ]
  , testGroup "line"
    [ testProperty "starts and ends at the start and end points" $ \start end ->
        let ℓ = line @Int start end
        in counterexample ("line: " <> show ℓ)
        $ length ℓ > 2 ==> (head ℓ === start) .&&. (head (reverse ℓ) === end)
    ]
  ]

--------------------------------------------------------------------------------
