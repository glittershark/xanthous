{-# LANGUAGE TemplateHaskell #-}

module Xanthous.Entities.Raws
  ( raws
  , raw
  ) where

import           Data.FileEmbed
import qualified Data.Yaml as Yaml
import           Xanthous.Prelude
import           System.FilePath.Posix

import           Xanthous.Entities.RawTypes

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
