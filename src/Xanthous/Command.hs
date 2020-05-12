--------------------------------------------------------------------------------
module Xanthous.Command where
--------------------------------------------------------------------------------
import Xanthous.Prelude hiding (Left, Right, Down)
--------------------------------------------------------------------------------
import Graphics.Vty.Input (Key(..), Modifier(..))
--------------------------------------------------------------------------------
import Xanthous.Data (Direction(..))
--------------------------------------------------------------------------------

data Command
  = Quit
  | Move Direction
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
  | Wield
  | GoUp
  | GoDown

    -- | TODO replace with `:` commands
  | ToggleRevealAll

commandFromKey :: Key -> [Modifier] -> Maybe Command
commandFromKey (KChar 'q') [] = Just Quit
commandFromKey (KChar '.') [] = Just Wait
commandFromKey (KChar (directionFromChar -> Just dir)) [] = Just $ Move dir
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
commandFromKey (KChar 'w') [] = Just Wield
commandFromKey (KChar '<') [] = Just GoUp
commandFromKey (KChar '>') [] = Just GoDown

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
