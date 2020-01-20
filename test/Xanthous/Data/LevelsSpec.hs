--------------------------------------------------------------------------------
module Xanthous.Data.LevelsSpec (main, test) where
--------------------------------------------------------------------------------
import Test.Prelude
--------------------------------------------------------------------------------
import qualified Data.Aeson as JSON
--------------------------------------------------------------------------------
import Xanthous.Util (between)
import Xanthous.Data.Levels
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Data.Levels"
  [ testGroup "current"
    [ testProperty "view is extract" $ \(levels :: Levels Int) ->
        levels ^. current === extract levels
    , testProperty "set replaces current" $ \(levels :: Levels Int) new ->
        extract (set current new levels) === new
    , testProperty "set extract is id" $ \(levels :: Levels Int) ->
        set current (extract levels) levels === levels
    , testProperty "set y ∘ set x ≡ set y" $ \(levels :: Levels Int) x y ->
        set current y (set current x levels) === set current y levels
    ]
  , localOption (QuickCheckTests 20)
  $ testBatch $ semigroup @(Levels Int) (error "unused", 1 :: Int)
  , testGroup "next/prev"
    [ testGroup "nextLevel"
      [ testProperty "seeks forwards" $ \(levels :: Levels Int) genned ->
          (pos . runIdentity . nextLevel (Identity genned) $ levels)
          === pos levels + 1
      , testProperty "maintains the invariant" $ \(levels :: Levels Int) genned ->
          let levels' = runIdentity . nextLevel (Identity genned) $ levels
          in between 0 (length levels') $ pos levels'
      , testProperty "extract is total" $ \(levels :: Levels Int) genned ->
          let levels' = runIdentity . nextLevel (Identity genned) $ levels
          in total $ extract levels'
      , testProperty "uses the generated level as the next level"
        $ \(levels :: Levels Int) genned ->
          let levels' = seek (length levels - 1) levels
              levels'' = runIdentity . nextLevel (Identity genned) $ levels'
          in counterexample (show levels'')
             $ extract levels'' === genned
      ]
    , testGroup "prevLevel"
      [ testProperty "seeks backwards" $ \(levels :: Levels Int) ->
          case prevLevel levels of
            Nothing -> property Discard
            Just levels' -> pos levels' === pos levels - 1
      , testProperty "maintains the invariant" $ \(levels :: Levels Int) ->
          case prevLevel levels of
            Nothing -> property Discard
            Just levels' -> property $ between 0 (length levels') $ pos levels'
      , testProperty "extract is total" $ \(levels :: Levels Int) ->
          case prevLevel levels of
            Nothing -> property Discard
            Just levels' -> total $ extract levels'
      ]
    ]
  , testGroup "JSON"
    [ testProperty "toJSON/parseJSON round-trip" $ \(levels :: Levels Int) ->
        JSON.decode (JSON.encode levels) === Just levels
    ]
  ]
