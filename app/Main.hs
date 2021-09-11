module Main ( main ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (finally)
import           Brick
import qualified Brick.BChan
import qualified Graphics.Vty as Vty
import qualified Options.Applicative as Opt
import           System.Random
import           Control.Monad.Random (getRandom)
import           Control.Exception (finally)
import           System.Exit (die)
--------------------------------------------------------------------------------
import qualified Xanthous.Game as Game
import           Xanthous.Game.Env (GameEnv(..))
import           Xanthous.App
import           Xanthous.Generators.Level
                 ( GeneratorInput
                 , parseGeneratorInput
                 , generateFromInput
                 , showCells
                 )
import qualified Xanthous.Entities.Character as Character
import           Xanthous.Generators.Level.Util (regions)
import           Xanthous.Generators.Level.LevelContents
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
  | Load FilePath
  | Generate GeneratorInput Dimensions (Maybe Int)

parseDimensions :: Opt.Parser Dimensions
parseDimensions = Dimensions
  <$> Opt.option Opt.auto
       ( Opt.short 'w'
       <> Opt.long "width"
       <> Opt.metavar "TILES"
       )
  <*> Opt.option Opt.auto
       ( Opt.short 'h'
       <> Opt.long "height"
       <> Opt.metavar "TILES"
       )


parseCommand :: Opt.Parser Command
parseCommand = (<|> Run <$> parseRunParams) $ Opt.subparser
  $ Opt.command "run"
      (Opt.info
       (Run <$> parseRunParams)
       (Opt.progDesc "Run the game"))
  <> Opt.command "load"
      (Opt.info
       (Load <$> Opt.argument Opt.str (Opt.metavar "FILE"))
       (Opt.progDesc "Load a saved game"))
  <> Opt.command "generate"
      (Opt.info
       (Generate
        <$> parseGeneratorInput
        <*> parseDimensions
        <*> optional
            (Opt.option Opt.auto (Opt.long "seed"))
        <**> Opt.helper
       )
       (Opt.progDesc "Generate a sample level"))

optParser :: Opt.ParserInfo Command
optParser = Opt.info
  (parseCommand <**> Opt.helper)
  (Opt.header "Xanthous: a WIP TUI RPG")

thanks :: IO ()
thanks = putStr "\n\n" >> putStrLn "Thanks for playing Xanthous!"

newGame :: RunParams -> IO ()
newGame rparams = do
  gameSeed <- maybe getRandom pure $ seed rparams
  when (isNothing $ seed rparams)
    . putStrLn
    $ "Seed: " <> tshow gameSeed
  let initialState = Game.initialStateFromSeed gameSeed &~ do
        for_ (characterName rparams) $ \cn ->
          Game.character . Character.characterName ?= cn
  runGame NewGame initialState `finally` do
    thanks
    when (isNothing $ seed rparams)
      . putStrLn
      $ "Seed: " <> tshow gameSeed
    putStr "\n\n"

loadGame :: FilePath -> IO ()
loadGame saveFile = do
  gameState <- maybe (die "Invalid save file!") pure
              =<< Game.loadGame . fromStrict <$> readFile @IO saveFile
  gameState `deepseq` runGame LoadGame gameState

runGame :: RunType -> Game.GameState -> IO ()
runGame rt gameState = do
  eventChan <- Brick.BChan.newBChan 10
  let gameEnv = GameEnv eventChan
  app <- makeApp gameEnv rt
  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  _game' <- customMain
    initialVty
    buildVty
    (Just eventChan)
    app
    gameState
  pure ()

runGenerate :: GeneratorInput -> Dimensions -> Maybe Int -> IO ()
runGenerate input dims mSeed = do
  putStrLn "Generating..."
  genSeed <- maybe getRandom pure mSeed
  let randGen = mkStdGen genSeed
      res = generateFromInput input dims randGen
      rs = regions $ amap not res
  when (isNothing mSeed)
    . putStrLn
    $ "Seed: " <> tshow genSeed
  putStr "num regions: "
  print $ length rs
  putStr "region lengths: "
  print $ length <$> rs
  putStr "character position: "
  print =<< chooseCharacterPosition res
  putStrLn $ showCells res

runCommand :: Command -> IO ()
runCommand (Run runParams) = newGame runParams
runCommand (Load saveFile) = loadGame saveFile
runCommand (Generate input dims mSeed) = runGenerate input dims mSeed

main :: IO ()
main = runCommand =<< Opt.execParser optParser
