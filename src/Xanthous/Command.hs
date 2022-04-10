{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Xanthous.Command
  ( -- * Commands
    Command(..)
  , commandIsHidden
    -- * Keybindings
  , Keybinding(..)
  , keybindings
  , commands
  , commandFromKey
  , directionFromChar
  ) where
--------------------------------------------------------------------------------
import Xanthous.Prelude hiding (Left, Right, Down, try)
--------------------------------------------------------------------------------
import           Graphics.Vty.Input (Key(..), Modifier(..))
import qualified Data.Char as Char
import           Data.Aeson (FromJSON (parseJSON), FromJSONKey, FromJSONKeyFunction (FromJSONKeyTextParser))
import qualified Data.Aeson as A
import           Data.Aeson.Generic.DerivingVia
import           Text.Megaparsec (Parsec, errorBundlePretty, parse, eof, try)
import           Text.Megaparsec.Char (string', char', printChar)
import           Data.FileEmbed (embedFile)
import qualified Data.Yaml as Yaml
import           Test.QuickCheck.Arbitrary
import           Data.Aeson.Types (Parser)
--------------------------------------------------------------------------------
import           Xanthous.Data (Direction(..))
import           Xanthous.Util.QuickCheck (GenericArbitrary(..))
--------------------------------------------------------------------------------

data Command
  = Quit
  | Help
  | Move !Direction
  | StartAutoMove !Direction
  | PreviousMessage
  | PickUp
  | Drop
  | Open
  | Close
  | Wait
  | Eat
  | Look
  | Save
  | Read
  | ShowInventory
  | DescribeInventory
  | Wield
  | Fire
  | GoUp
  | GoDown
  | Rest

    -- | TODO replace with `:` commands
  | ToggleRevealAll
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData)
  deriving Arbitrary via GenericArbitrary Command
  deriving (FromJSON)
       via WithOptions '[ SumEnc UntaggedVal ]
           Command

-- | Should the command be hidden from the help menu?
--
-- Note that this is true for both debug commands and movement commands, as the
-- latter is documented non-automatically
commandIsHidden :: Command -> Bool
commandIsHidden (Move _) = True
commandIsHidden (StartAutoMove _) = True
commandIsHidden ToggleRevealAll = True
commandIsHidden _ = False

--------------------------------------------------------------------------------

data Keybinding = Keybinding !Key ![Modifier]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData)

parseKeybindingFromText :: Text -> Parser Keybinding
parseKeybindingFromText
  = either (fail . errorBundlePretty) pure
  . parse keybinding "<JSON>"
  where
    key :: Parsec Void Text Key
    key = KUp <$ string' "<up>"
      <|> KDown <$ string' "<down>"
      <|> KLeft <$ string' "<left>"
      <|> KRight <$ string' "<right>"
      <|> KChar <$> printChar

    modifier :: Parsec Void Text Modifier
    modifier = modf <* char' '-'
      where
        modf = MAlt <$ char' 'a'
          <|> MMeta <$ char' 'm'
          <|> MCtrl  <$ char' 'c'
          <|> MShift  <$ char' 's'

    keybinding :: Parsec Void Text Keybinding
    keybinding = do
      mods <- many (try modifier)
      k <- key
      eof
      pure $ Keybinding k mods

instance FromJSON Keybinding where
  parseJSON = A.withText "Keybinding" parseKeybindingFromText

instance FromJSONKey Keybinding where
  fromJSONKey = FromJSONKeyTextParser parseKeybindingFromText

rawKeybindings :: ByteString
rawKeybindings = $(embedFile "src/Xanthous/keybindings.yaml")

keybindings :: HashMap Keybinding Command
keybindings = either (error . Yaml.prettyPrintParseException) id
  $ Yaml.decodeEither' rawKeybindings

commands :: HashMap Command Keybinding
commands = mapFromList . map swap . itoList $ keybindings

commandFromKey :: Key -> [Modifier] -> Maybe Command
commandFromKey (KChar (directionFromChar -> Just dir)) [] = Just $ Move dir
commandFromKey (KChar c) []
  | Char.isUpper c
  , Just dir <- directionFromChar $ Char.toLower c
  = Just $ StartAutoMove dir
commandFromKey k mods = keybindings ^. at keybinding
  where keybinding = Keybinding k mods

--------------------------------------------------------------------------------

directionFromChar :: Char -> Maybe Direction
directionFromChar 'h' = Just Left
directionFromChar 'j' = Just Down
directionFromChar 'k' = Just Up
directionFromChar 'l' = Just Right
directionFromChar 'y' = Just UpLeft
directionFromChar 'u' = Just UpRight
directionFromChar 'b' = Just DownLeft
directionFromChar 'n' = Just DownRight
directionFromChar '.' = Just Here
directionFromChar _   = Nothing
