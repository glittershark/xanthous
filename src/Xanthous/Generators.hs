{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Xanthous.Generators
  ( generate
  , SGenerator(..)
  , GeneratorInput
  , generateFromInput
  , parseGeneratorInput
  , showCells
  , Level(..)
  , levelWalls
  , levelItems
  , levelCreatures
  , levelCharacterPosition
  , generateLevel
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (Level)
import           Data.Array.Unboxed
import           System.Random (RandomGen)
import qualified Options.Applicative as Opt
import           Control.Monad.Random
--------------------------------------------------------------------------------
import qualified Xanthous.Generators.CaveAutomata as CaveAutomata
import           Xanthous.Generators.Util
import           Xanthous.Generators.LevelContents
import           Xanthous.Data (Dimensions, Position'(Position), Position)
import           Xanthous.Data.EntityMap (EntityMap)
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Entities.Environment
import           Xanthous.Entities.Item (Item)
import           Xanthous.Entities.Creature (Creature)
--------------------------------------------------------------------------------

data Generator = CaveAutomata
  deriving stock (Show, Eq)

data SGenerator (gen :: Generator) where
  SCaveAutomata :: SGenerator 'CaveAutomata

type family Params (gen :: Generator) :: Type where
  Params 'CaveAutomata = CaveAutomata.Params

generate
  :: RandomGen g
  => SGenerator gen
  -> Params gen
  -> Dimensions
  -> g
  -> Cells
generate SCaveAutomata = CaveAutomata.generate

data GeneratorInput where
  GeneratorInput :: forall gen. SGenerator gen -> Params gen -> GeneratorInput

generateFromInput :: RandomGen g => GeneratorInput -> Dimensions -> g -> Cells
generateFromInput (GeneratorInput sg ps) = generate sg ps

parseGeneratorInput :: Opt.Parser GeneratorInput
parseGeneratorInput = Opt.subparser $
  Opt.command "cave" (Opt.info
                      (GeneratorInput <$> pure SCaveAutomata <*> CaveAutomata.parseParams)
                      (Opt.progDesc "cellular-automata based cave generator"))

showCells :: Cells -> Text
showCells arr =
  let ((minX, minY), (maxX, maxY)) = bounds arr
      showCellVal True = "x"
      showCellVal False = " "
      showCell = showCellVal . (arr !)
      row r = foldMap (showCell . (, r)) [minX..maxX]
      rows = row <$> [minY..maxY]
  in intercalate "\n" rows

cellsToWalls :: Cells -> EntityMap Wall
cellsToWalls cells = foldl' maybeInsertWall mempty . assocs $ cells
  where
    maybeInsertWall em (pos@(x, y), True)
      | not (surroundedOnAllSides pos) =
        let x' = fromIntegral x
            y' = fromIntegral y
        in EntityMap.insertAt (Position x' y') Wall em
    maybeInsertWall em _ = em
    surroundedOnAllSides pos = numAliveNeighbors cells pos == 8

--------------------------------------------------------------------------------

data Level = Level
  { _levelWalls             :: !(EntityMap Wall)
  , _levelItems             :: !(EntityMap Item)
  , _levelCreatures         :: !(EntityMap Creature)
  , _levelCharacterPosition :: !Position
  }
makeLenses ''Level

generateLevel :: MonadRandom m => SGenerator gen -> Params gen -> Dimensions -> m Level
generateLevel gen ps dims = do
  rand <- mkStdGen <$> getRandom
  let cells = generate gen ps dims rand
      _levelWalls = cellsToWalls cells
  _levelItems <- randomItems cells
  _levelCreatures <- randomCreatures cells
  _levelCharacterPosition <- chooseCharacterPosition cells
  pure Level {..}
