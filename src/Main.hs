module Main where

import Xanthous.Prelude
import Brick

import Xanthous.Game (getInitialState)
import Xanthous.App (makeApp)

ui :: Widget ()
ui = str "Hello, world!"

main :: IO ()
main = do
  app <- makeApp
  let initialState = getInitialState
  _ <- defaultMain app initialState
  pure ()
