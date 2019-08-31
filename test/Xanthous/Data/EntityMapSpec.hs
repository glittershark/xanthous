{-# LANGUAGE ApplicativeDo #-}
--------------------------------------------------------------------------------
module Xanthous.Data.EntityMapSpec where
--------------------------------------------------------------------------------
import Test.Prelude
--------------------------------------------------------------------------------
import Xanthous.Data.EntityMap
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Data.EntityMap"
  [ testBatch $ monoid @(EntityMap Int) mempty
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
  ]
