{-# LANGUAGE ApplicativeDo #-}
--------------------------------------------------------------------------------
module Xanthous.Data.EntityMapSpec where
--------------------------------------------------------------------------------
import           Test.Prelude
import           Control.Lens.Properties
--------------------------------------------------------------------------------
import qualified Data.Aeson as JSON
--------------------------------------------------------------------------------
import           Xanthous.Data.EntityMap
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = localOption (QuickCheckTests 20)
  $ testGroup "Xanthous.Data.EntityMap"
  [ testBatch $ monoid @(EntityMap Int) mempty
  , testGroup "Deduplicate"
    [ testGroup "Semigroup laws"
      [ testProperty "associative" $ \(a :: Deduplicate (EntityMap Int)) b c ->
          a <> (b <> c) === (a <> b) <> c
      ]
    ]
  , testGroup "Eq laws"
    [ testProperty "reflexivity" $ \(em :: EntityMap Int) ->
        em == em
    , testProperty "symmetric" $ \(em₁ :: EntityMap Int) em₂ ->
        (em₁ == em₂) == (em₂ == em₁)
    , testProperty "transitive" $ \(em₁ :: EntityMap Int) em₂ em₃ ->
        if (em₁ == em₂ && em₂ == em₃)
        then (em₁ == em₃)
        else True
    ]
  , testGroup "JSON encoding/decoding"
    [ testProperty "round-trips" $ \(em :: EntityMap Int) ->
        let em' = JSON.decode (JSON.encode em)
        in counterexample (show (em' ^? _Just . lastID, em ^. lastID
                                , em' ^? _Just . byID == em ^. byID . re _Just
                                , em' ^? _Just . byPosition == em ^. byPosition . re _Just
                                , em' ^? _Just . _EntityMap == em ^. _EntityMap . re _Just
                                ))
           $ em' === Just em
    , testProperty "Preserves IDs" $ \(em :: EntityMap Int) ->
        let Just em' = JSON.decode $ JSON.encode em
        in toEIDsAndPositioned em' === toEIDsAndPositioned em
    ]

  , testGroup "atPosition"
    [ testProperty "lens laws" $ \pos -> isLens $ atPosition @Int pos
    ]
  ]
