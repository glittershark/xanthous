{-# LANGUAGE TemplateHaskell #-}
module Xanthous.Messages
  ( Message(..)
  , resolve
  , MessageMap(..)
  , lookupMessage

    -- * Game messages
  , messages
  , message
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude

import Control.Monad.Random.Class (MonadRandom)
import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Aeson.Generic.DerivingVia
import Data.FileEmbed
import Data.List.NonEmpty
import Test.QuickCheck hiding (choose)
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances.UnorderedContainers ()
import Text.Mustache
import qualified Data.Yaml as Yaml

import Xanthous.Random
import Xanthous.Orphans ()

data Message = Single Template | Choice (NonEmpty Template)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (CoArbitrary, Function, NFData)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ SumEnc UntaggedVal ]
           Message

instance Arbitrary Message where
  arbitrary = genericArbitrary
  shrink = genericShrink

resolve :: MonadRandom m => Message -> m Template
resolve (Single t) = pure t
resolve (Choice ts) = choose ts

data MessageMap = Direct Message | Nested (HashMap Text MessageMap)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (CoArbitrary, Function, NFData)
  deriving (ToJSON, FromJSON)
       via WithOptions '[ SumEnc UntaggedVal ]
           MessageMap

instance Arbitrary MessageMap where
  arbitrary = frequency [ (10, Direct <$> arbitrary)
                        , (1, Nested <$> arbitrary)
                        ]

lookupMessage :: [Text] -> MessageMap -> Maybe Message
lookupMessage [] (Direct msg) = Just msg
lookupMessage (k:ks) (Nested m) = lookupMessage ks =<< m ^. at k
lookupMessage _ _ = Nothing

type instance Index MessageMap = [Text]
type instance IxValue MessageMap = Message
instance Ixed MessageMap where
  ix [] f (Direct msg) = Direct <$> f msg
  ix (k:ks) f (Nested m) = case m ^. at k of
    Just m' -> ix ks f m' <&> \m'' ->
      Nested $ m & at k ?~ m''
    Nothing -> pure $ Nested m
  ix _ _ m = pure m

--------------------------------------------------------------------------------

rawMessages :: ByteString
rawMessages = $(embedFile "src/Xanthous/messages.yaml")

messages :: MessageMap
messages
  = either (error . Yaml.prettyPrintParseException) id
  $ Yaml.decodeEither' rawMessages

message :: (MonadRandom m, ToJSON params) => [Text] -> params -> m Text
message path params = maybe notFound renderMessage $ messages ^? ix path
  where
    renderMessage msg = do
      tpl <- resolve msg
      pure . toStrict . renderMustache tpl $ toJSON params
    notFound = pure "Message not found"
