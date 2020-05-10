module Xanthous.Util.GraphicsSpec (main, test) where
--------------------------------------------------------------------------------
import Test.Prelude hiding (head)
--------------------------------------------------------------------------------
import Xanthous.Util.Graphics
import Xanthous.Util
import Data.List (head)
import Data.Set (isSubsetOf)
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
      $ (sort . unique @[] @[_]) (circle @Int (2, 2) 1)
      @?= [ (1, 2)
          , (2, 1), (2, 3)
          , (3, 2)
          ]
    , testCase "radius 12, origin 0"
      $ (sort . unique @[] @[_]) (circle @Int (0, 0) 12)
      @?= [ (-12,-4),(-12,-3),(-12,-2),(-12,-1),(-12,0),(-12,1),(-12,2)
          , (-12,3),(-12,4),(-11,-6),(-11,-5),(-11,5),(-11,6),(-10,-7),(-10,7)
          , (-9,-9),(-9,-8),(-9,8),(-9,9),(-8,-9),(-8,9),(-7,-10),(-7,10)
          , (-6,-11),(-6,11),(-5,-11),(-5 ,11),(-4,-12),(-4,12),(-3,-12),(-3,12)
          , (-2,-12),(-2,12),(-1,-12),(-1,12),(0,-12),(0,12),(1,-12),(1,12)
          , (2,-12),(2,12),(3,-12),(3,12),(4,-12),(4,12),(5,-11),(5 ,11),(6,-11)
          , (6,11),(7,-10),(7,10),(8,-9),(8,9),(9,-9),(9,-8),(9,8),(9,9),(10,-7)
          , (10,7),(11,-6),(11,-5),(11,5),(11,6),(12,-4),(12,-3),(12,-2),(12,-1)
          , (12,0), (12,1),(12,2),(12,3),(12,4)
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
