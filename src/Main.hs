module Main ( main ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (finally)
import           Brick
import qualified Options.Applicative as Opt
import           System.Random
import           Control.Monad.Random (getRandom)
import           Control.Exception (finally)
--------------------------------------------------------------------------------
import qualified Xanthous.Game as Game
import           Xanthous.App (makeApp)
import           Xanthous.Generators
                 ( GeneratorInput
                 , parseGeneratorInput
                 , generateFromInput
                 , showCells
                 )
import qualified Xanthous.Entities.Character as Character
import           Xanthous.Generators.Util (regions)
import           Xanthous.Generators.LevelContents
import           Xanthous.Data (Dimensions, Dimensions'(Dimensions))
import           Data.Array.IArray ( amap )
--------------------------------------------------------------------------------

data RunParams = RunParams
  { seed :: Maybe Int
  , characterName :: Maybe Text
  }
  deriving stock (Show, Eq)

parseRunParams :: Opt.Parser RunParams
parseRunParams = RunParams
  <$> optional (Opt.option Opt.auto
      ( Opt.long "seed"
      <> Opt.help "Random seed for the game."
      ))
  <*> optional (Opt.strOption
      ( Opt.short 'n'
      <> Opt.long "name"
      <> Opt.help
        ( "Name for the character. If not set on the command line, "
        <> "will be prompted for at runtime"
        )
      ))

data Command
  = Run RunParams
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
parseCommand = (<|> Run <$> parseRunParams) $ Opt.subparser
  $ Opt.command "run"
      (Opt.info
       (Run <$> parseRunParams)
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

runGame :: RunParams -> IO ()
runGame rparams = do
  app <- makeApp
  gameSeed <- maybe getRandom pure $ seed rparams
  let initialState = Game.initialStateFromSeed gameSeed &~ do
        for_ (characterName rparams) $ \cn ->
          Game.character . Character.characterName ?= cn
  _game' <- defaultMain app initialState `finally` do
    putStr "\n\n"
    putStrLn "Thanks for playing Xanthous!"
    when (isNothing $ seed rparams)
      . putStrLn
      $ "Seed: " <> tshow gameSeed
    putStr "\n\n"
  pure ()

runGenerate :: GeneratorInput -> Dimensions -> IO ()
runGenerate input dims = do
  randGen <- getStdGen
  let res = generateFromInput input dims randGen
      rs = regions $ amap not res
  putStr "num regions: "
  print $ length rs
  putStr "region lengths: "
  print $ length <$> rs
  putStr "character position: "
  print =<< chooseCharacterPosition res
  putStrLn $ showCells res

runCommand :: Command -> IO ()
runCommand (Run runParams) = runGame runParams
runCommand (Generate input dims) = runGenerate input dims

main :: IO ()
main = runCommand =<< Opt.execParser optParser
