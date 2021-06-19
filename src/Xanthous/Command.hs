--------------------------------------------------------------------------------
module Xanthous.Command where
--------------------------------------------------------------------------------
import Xanthous.Prelude hiding (Left, Right, Down)
--------------------------------------------------------------------------------
import Graphics.Vty.Input (Key(..), Modifier(..))
import qualified Data.Char as Char
--------------------------------------------------------------------------------
import Xanthous.Data (Direction(..))
--------------------------------------------------------------------------------

data Command
  = Quit
  | Move Direction
  | StartAutoMove Direction
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
  | GoUp
  | GoDown
  | Rest

    -- | TODO replace with `:` commands
  | ToggleRevealAll

commandFromKey :: Key -> [Modifier] -> Maybe Command
commandFromKey (KChar 'q') [] = Just Quit
commandFromKey (KChar '.') [] = Just Wait
commandFromKey (KChar (directionFromChar -> Just dir)) [] = Just $ Move dir
commandFromKey (KChar c) []
  | Char.isUpper c
  , Just dir <- directionFromChar $ Char.toLower c
  = Just $ StartAutoMove dir
commandFromKey (KChar 'p') [MCtrl] = Just PreviousMessage
commandFromKey (KChar ',') [] = Just PickUp
commandFromKey (KChar 'd') [] = Just Drop
commandFromKey (KChar 'o') [] = Just Open
commandFromKey (KChar 'c') [] = Just Close
commandFromKey (KChar ';') [] = Just Look
commandFromKey (KChar 'e') [] = Just Eat
commandFromKey (KChar 'S') [] = Just Save
commandFromKey (KChar 'r') [] = Just Read
commandFromKey (KChar 'i') [] = Just ShowInventory
commandFromKey (KChar 'I') [] = Just DescribeInventory
commandFromKey (KChar 'w') [] = Just Wield
commandFromKey (KChar '<') [] = Just GoUp
commandFromKey (KChar '>') [] = Just GoDown
commandFromKey (KChar 'R') [] = Just Rest

-- DEBUG COMMANDS --
commandFromKey (KChar 'r') [MMeta] = Just ToggleRevealAll

commandFromKey _ _ = Nothing

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
