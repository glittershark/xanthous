-- |

module Xanthous.DataSpec (main, test) where

import Test.Prelude hiding (Right, Left, Down)
import Xanthous.Data
import Data.Group

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Data"
  [ testGroup "Position"
    [ testBatch $ monoid @Position mempty
    , testProperty "group laws" $ \(pos :: Position) ->
        pos <> invert pos == mempty && invert pos <> pos == mempty
    ]
  , testGroup "Direction"
    [ testProperty "opposite is involutive" $ \(dir :: Direction) ->
        opposite (opposite dir) == dir
    , testProperty "opposite provides inverse" $ \dir ->
        invert (asPosition dir) == asPosition (opposite dir)
    , testGroup "Move"
      [ testCase "Up"        $ move Up mempty        @?= Position 0 (-1)
      , testCase "Down"      $ move Down mempty      @?= Position 0 1
      , testCase "Left"      $ move Left mempty      @?= Position (-1) 0
      , testCase "Right"     $ move Right mempty     @?= Position 1 0
      , testCase "UpLeft"    $ move UpLeft mempty    @?= Position (-1) (-1)
      , testCase "UpRight"   $ move UpRight mempty   @?= Position 1 (-1)
      , testCase "DownLeft"  $ move DownLeft mempty  @?= Position (-1) 1
      , testCase "DownRight" $ move DownRight mempty @?= Position 1 1
      ]
    ]
  ]
