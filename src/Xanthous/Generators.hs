{-# LANGUAGE GADTs #-}
--------------------------------------------------------------------------------
module Xanthous.Generators where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
import           Data.Array.Unboxed
import           System.Random (RandomGen)
import qualified Options.Applicative as Opt
--------------------------------------------------------------------------------
import qualified Xanthous.Generators.CaveAutomata as CaveAutomata
import           Xanthous.Generators.Util
import           Xanthous.Data (Dimensions, Position(Position))
import           Xanthous.Data.EntityMap (EntityMap)
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Entities.Environment
--------------------------------------------------------------------------------

data Generator = CaveAutomata
  deriving stock (Show, Eq)

data SGenerator (gen :: Generator) where
  SCaveAutomata :: SGenerator 'CaveAutomata

data AGenerator where
  AGenerator :: forall gen. SGenerator gen -> AGenerator

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
