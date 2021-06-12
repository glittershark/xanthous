{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Xanthous.Generators.Level
  ( generate
  , Generator(..)
  , SGenerator(..)
  , GeneratorInput(..)
  , generateFromInput
  , parseGeneratorInput
  , showCells
  , Level(..)
  , levelWalls
  , levelItems
  , levelCreatures
  , levelDoors
  , levelCharacterPosition
  , levelTutorialMessage
  , levelExtra
  , generateLevel
  , levelToEntityMap
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
import           Data.Array.Unboxed
import qualified Options.Applicative as Opt
import           Control.Monad.Random
--------------------------------------------------------------------------------
import qualified Xanthous.Generators.Level.CaveAutomata as CaveAutomata
import qualified Xanthous.Generators.Level.Dungeon as Dungeon
import           Xanthous.Generators.Level.Util
import           Xanthous.Generators.Level.LevelContents
import           Xanthous.Generators.Level.Village as Village
import           Xanthous.Data (Dimensions, Position'(Position), Position)
import           Xanthous.Data.EntityMap (EntityMap, _EntityMap)
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Entities.Environment
import           Xanthous.Entities.Item (Item)
import           Xanthous.Entities.Creature (Creature)
import           Xanthous.Game.State (SomeEntity(..))
import           Linear.V2
--------------------------------------------------------------------------------

data Generator
  = CaveAutomata
  | Dungeon
  deriving stock (Show, Eq)

data SGenerator (gen :: Generator) where
  SCaveAutomata :: SGenerator 'CaveAutomata
  SDungeon :: SGenerator 'Dungeon

type family Params (gen :: Generator) :: Type where
  Params 'CaveAutomata = CaveAutomata.Params
  Params 'Dungeon = Dungeon.Params

generate
  :: RandomGen g
  => SGenerator gen
  -> Params gen
  -> Dimensions
  -> g
  -> Cells
generate SCaveAutomata = CaveAutomata.generate
generate SDungeon = Dungeon.generate

data GeneratorInput where
  GeneratorInput :: forall gen. SGenerator gen -> Params gen -> GeneratorInput

generateFromInput :: RandomGen g => GeneratorInput -> Dimensions -> g -> Cells
generateFromInput (GeneratorInput sg ps) = generate sg ps

parseGeneratorInput :: Opt.Parser GeneratorInput
parseGeneratorInput = Opt.subparser
  $ generatorCommand SCaveAutomata
      "cave"
      "Cellular-automata based cave generator"
      CaveAutomata.parseParams
  <> generatorCommand SDungeon
      "dungeon"
      "Classic dungeon map generator"
      Dungeon.parseParams
  where
    generatorCommand sgen name desc parseParams =
      Opt.command name
        (Opt.info
          (GeneratorInput <$> pure sgen <*> parseParams)
          (Opt.progDesc desc)
        )


showCells :: Cells -> Text
showCells arr =
  let (V2 minX minY, V2 maxX maxY) = bounds arr
      showCellVal True = "x"
      showCellVal False = " "
      showCell = showCellVal . (arr !)
      row r = foldMap (showCell . (`V2` r)) [minX..maxX]
      rows = row <$> [minY..maxY]
  in intercalate "\n" rows

cellsToWalls :: Cells -> EntityMap Wall
cellsToWalls cells = foldl' maybeInsertWall mempty . assocs $ cells
  where
    maybeInsertWall em (pos@(V2 x y), True)
      | not (surroundedOnAllSides pos) =
        let x' = fromIntegral x
            y' = fromIntegral y
        in EntityMap.insertAt (Position x' y') Wall em
    maybeInsertWall em _ = em
    surroundedOnAllSides pos = numAliveNeighbors cells pos == 8

--------------------------------------------------------------------------------

data Level = Level
  { _levelWalls             :: !(EntityMap Wall)
  , _levelDoors             :: !(EntityMap Door)
  , _levelItems             :: !(EntityMap Item)
  , _levelCreatures         :: !(EntityMap Creature)
  , _levelTutorialMessage   :: !(EntityMap GroundMessage)
  , _levelStaircases        :: !(EntityMap Staircase)
  , _levelExtra             :: !(EntityMap SomeEntity) -- ^ TODO this is a bit of a hack...
  , _levelCharacterPosition :: !Position
  }
  deriving stock (Generic)
  deriving anyclass (NFData)
makeLenses ''Level

generateLevel
  :: MonadRandom m
  => SGenerator gen
  -> Params gen
  -> Dimensions
  -> m Level
generateLevel gen ps dims = do
  rand <- mkStdGen <$> getRandom
  let cells = generate gen ps dims rand
      _levelWalls = cellsToWalls cells
  village <- generateVillage cells gen
  let _levelExtra = village
  _levelItems <- randomItems cells
  _levelCreatures <- randomCreatures cells
  _levelDoors <- randomDoors cells
  _levelCharacterPosition <- chooseCharacterPosition cells
  let upStaircase = _EntityMap # [(_levelCharacterPosition, UpStaircase)]
  downStaircase <- placeDownStaircase cells
  let _levelStaircases = upStaircase <> downStaircase
  _levelTutorialMessage <- tutorialMessage cells _levelCharacterPosition
  pure Level {..}

levelToEntityMap :: Level -> EntityMap SomeEntity
levelToEntityMap level
  = (SomeEntity <$> level ^. levelWalls)
  <> (SomeEntity <$> level ^. levelDoors)
  <> (SomeEntity <$> level ^. levelItems)
  <> (SomeEntity <$> level ^. levelCreatures)
  <> (SomeEntity <$> level ^. levelTutorialMessage)
  <> (SomeEntity <$> level ^. levelStaircases)
  <> (level ^. levelExtra)

generateVillage
  :: MonadRandom m
  => Cells -- ^ Wall positions
  -> SGenerator gen
  -> m (EntityMap SomeEntity)
generateVillage wallPositions SCaveAutomata = Village.fromCave wallPositions
generateVillage _ _ = pure mempty
