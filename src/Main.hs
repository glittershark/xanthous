module Main where

import Xanthous.Prelude
import Brick
import qualified Options.Applicative as Opt
import System.Random

import Xanthous.Game (getInitialState)
import Xanthous.App (makeApp)
import Xanthous.Generators
  ( GeneratorInput(..)
  , parseGeneratorInput
  , generateFromInput
  , showCells
  )
import Xanthous.Data (Dimensions, Dimensions'(Dimensions))

data Command
  = Run
  | Generate GeneratorInput Dimensions

parseDimensions :: Opt.Parser Dimensions
parseDimensions = Dimensions
  <$> Opt.option Opt.auto
       ( Opt.short 'w'
       <> Opt.long "width"
       )
  <*> Opt.option Opt.auto
       ( Opt.short 'h'
       <> Opt.long "height"
       )

parseCommand :: Opt.Parser Command
parseCommand = (<|> pure Run) $ Opt.subparser
  $ Opt.command "run"
      (Opt.info
       (pure Run)
       (Opt.progDesc "Run the game"))
  <> Opt.command "generate"
      (Opt.info
       (Generate
        <$> parseGeneratorInput
        <*> parseDimensions
        <**> Opt.helper
       )
       (Opt.progDesc "Generate a sample level"))

optParser :: Opt.ParserInfo Command
optParser = Opt.info
  (parseCommand <**> Opt.helper)
  (Opt.header "Xanthous: a WIP TUI RPG")

runGame :: IO ()
runGame =  do
  app <- makeApp
  initialState <- getInitialState
  _ <- defaultMain app initialState
  pure ()

runGenerate :: GeneratorInput -> Dimensions -> IO ()
runGenerate input dims = do
  randGen <- getStdGen
  let res = generateFromInput input dims randGen
  putStrLn $ showCells res

runCommand :: Command -> IO ()
runCommand Run = runGame
runCommand (Generate input dims) = runGenerate input dims

main :: IO ()
main = runCommand =<< Opt.execParser optParser
