{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Xanthous.Entities.Raws
  ( raws
  , raw
  , RawType(..)
  , rawsWithType
  , entityFromRaw
  ) where
--------------------------------------------------------------------------------
import           Data.FileEmbed
import qualified Data.Yaml as Yaml
import           Xanthous.Prelude
import           System.FilePath.Posix
--------------------------------------------------------------------------------
import           Xanthous.Entities.RawTypes
import           Xanthous.Entities
import qualified Xanthous.Entities.Creature as Creature
import qualified Xanthous.Entities.Item as Item
import           Xanthous.AI.Gormlak ()
--------------------------------------------------------------------------------
rawRaws :: [(FilePath, ByteString)]
rawRaws = $(embedDir "src/Xanthous/Entities/Raws")

raws :: HashMap Text EntityRaw
raws
  = mapFromList
  . map (bimap
         (pack . takeBaseName)
         (either (error . Yaml.prettyPrintParseException) id
          . Yaml.decodeEither'))
  $ rawRaws

raw :: Text -> Maybe EntityRaw
raw n = raws ^. at n

class RawType (a :: Type) where
  _RawType :: Prism' EntityRaw a

instance RawType CreatureType where
  _RawType = prism' Creature $ \case
    Creature c -> Just c
    _ -> Nothing

instance RawType ItemType where
  _RawType = prism' Item $ \case
    Item i -> Just i
    _ -> Nothing

rawsWithType :: forall a. RawType a => HashMap Text a
rawsWithType = mapFromList . itoListOf (ifolded . _RawType) $ raws

--------------------------------------------------------------------------------

entityFromRaw :: EntityRaw -> SomeEntity
entityFromRaw (Creature creatureType)
  = SomeEntity $ Creature.newWithType creatureType
entityFromRaw (Item itemType)
  = SomeEntity $ Item.newWithType itemType
