--------------------------------------------------------------------------------
module Xanthous.DataSpec (main, test) where
--------------------------------------------------------------------------------
import Test.Prelude hiding (Right, Left, Down, toList, all)
import Data.Group
import Data.Foldable (toList, all)
--------------------------------------------------------------------------------
import Xanthous.Data
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Data"
  [ testGroup "Position"
    [ testBatch $ monoid @Position mempty
    , testProperty "group laws" $ \(pos :: Position) ->
        pos <> invert pos == mempty && invert pos <> pos == mempty
    , testGroup "stepTowards laws"
      [ testProperty "takes only one step" $ \src tgt ->
          src /= tgt ==>
            isUnit (src `diffPositions` (src `stepTowards` tgt))
      -- , testProperty "moves in the right direction" $ \src tgt ->
      --     stepTowards src tgt == move (directionOf src tgt) src
      ]
    , testProperty "directionOf laws" $ \pos dir ->
        directionOf pos (move dir pos) == dir
    , testProperty "diffPositions is add inverse" $ \(pos₁ :: Position) pos₂ ->
        diffPositions pos₁ pos₂ == addPositions pos₁ (invert pos₂)
    , testGroup "isUnit"
      [ testProperty "double direction is never unit" $ \dir ->
          not . isUnit $ move dir (asPosition dir)
      , testCase "examples" $ do
          isUnit (Position @Int 1 1) @? "not . isUnit $ Position 1 1"
          isUnit (Position @Int 0 (-1)) @? "not . isUnit $ Position 0 (-1)"
          (not . isUnit) (Position @Int 1 13) @? "isUnit $ Position 1 13"
      ]
    ]

  , testGroup "Direction"
    [ testProperty "opposite is involutive" $ \(dir :: Direction) ->
        opposite (opposite dir) == dir
    , testProperty "opposite provides inverse" $ \dir ->
        invert (asPosition dir) === asPosition (opposite dir)
    , testProperty "asPosition isUnit" $ \dir ->
        dir /= Here ==> isUnit (asPosition dir)
    , testGroup "Move"
      [ testCase "Up"        $ move Up mempty        @?= Position @Int 0 (-1)
      , testCase "Down"      $ move Down mempty      @?= Position @Int 0 1
      , testCase "Left"      $ move Left mempty      @?= Position @Int (-1) 0
      , testCase "Right"     $ move Right mempty     @?= Position @Int 1 0
      , testCase "UpLeft"    $ move UpLeft mempty    @?= Position @Int (-1) (-1)
      , testCase "UpRight"   $ move UpRight mempty   @?= Position @Int 1 (-1)
      , testCase "DownLeft"  $ move DownLeft mempty  @?= Position @Int (-1) 1
      , testCase "DownRight" $ move DownRight mempty @?= Position @Int 1 1
      ]
    ]

  , testGroup "Corner"
    [ testGroup "instance Opposite"
      [ testProperty "involutive" $ \(corner :: Corner) ->
          opposite (opposite corner) === corner
      ]
    ]

  , testGroup "Edge"
    [ testGroup "instance Opposite"
      [ testProperty "involutive" $ \(edge :: Edge) ->
          opposite (opposite edge) === edge
      ]
    ]

  , testGroup "Box"
    [ testGroup "boxIntersects"
      [ testProperty "True" $ \dims ->
          boxIntersects (Box @Word (V2 1 1) (V2 2 2))
                        (Box (V2 2 2) dims)
      , testProperty "False" $ \dims ->
          not $ boxIntersects (Box @Word (V2 1 1) (V2 2 2))
                            (Box (V2 4 2) dims)
      ]
    ]

  , testGroup "Neighbors"
    [ testGroup "rotations"
      [ testProperty "always has the same members"
        $ \(neighs :: Neighbors Int) ->
          all (\ns -> sort (toList ns) == sort (toList neighs))
          $ rotations neighs
      , testProperty "all rotations have the same rotations"
        $ \(neighs :: Neighbors Int) ->
          let rots = rotations neighs
          in all (\ns -> sort (toList $ rotations ns) == sort (toList rots))
             rots
      ]
    ]

  , testGroup "units"
    [ testGroup "unit suffixes"
      [ testCase "density"
        $ tshow (10000 :: Grams `Per` Cubic Meters)
          @?= "10000.0 g/m³"
      ]
    ]
  ]
