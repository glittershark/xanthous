{-# LANGUAGE ApplicativeDo #-}
--------------------------------------------------------------------------------
module Xanthous.Data.EntityMapSpec where
--------------------------------------------------------------------------------
import           Test.Prelude
--------------------------------------------------------------------------------
import qualified Data.Aeson as JSON
--------------------------------------------------------------------------------
import           Xanthous.Data.EntityMap
import           Xanthous.Data (Positioned(..))
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

  , localOption (QuickCheckTests 50)
  $ testGroup "atPosition"
    [ testProperty "setget" $ \pos (em :: EntityMap Int) es ->
        view (atPosition pos) (set (atPosition pos) es em) === es
    , testProperty "getset" $ \pos (em :: EntityMap Int) ->
        set (atPosition pos) (view (atPosition pos) em) em === em
    , testProperty "setset" $ \pos (em :: EntityMap Int) es ->
        (set (atPosition pos) es . set (atPosition pos) es) em
        ===
        set (atPosition pos) es em
      -- testProperty "lens laws" $ \pos -> isLens $ atPosition @Int pos
    , testProperty "preserves IDs" $ \(em :: EntityMap Int) e1 e2 p ->
        let (eid, em') = insertAtReturningID p e1 em
            em'' = em' & atPosition p %~ (e2 <|)
        in
          counterexample ("em': " <> show em')
          . counterexample ("em'': " <> show em'')
          $ em'' ^. at eid === Just (Positioned p e1)
    ]
  ]
