-- |

module Xanthous.Entities.RawsSpec (main, test) where

import Test.Prelude
import Xanthous.Entities.Raws

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "Xanthous.Entities.Raws"
  [ testGroup "raws"
    [ testCase "are all valid" $ raws `deepseq` pure ()
    ]
  ]
