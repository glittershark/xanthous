{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Xanthous.Generators.Level.CaveAutomata
  ( Params(..)
  , defaultParams
  , parseParams
  , generate
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
import           Control.Monad.Random (RandomGen, runRandT)
import           Data.Array.ST
import           Data.Array.Unboxed
import qualified Options.Applicative as Opt
--------------------------------------------------------------------------------
import           Xanthous.Util (between)
import           Xanthous.Util.Optparse
import           Xanthous.Data (Dimensions, width, height)
import           Xanthous.Generators.Level.Util
import           Linear.V2
--------------------------------------------------------------------------------

data Params = Params
  { _aliveStartChance :: Double
  , _birthLimit :: Word
  , _deathLimit :: Word
  , _steps :: Word
  }
  deriving stock (Show, Eq, Generic)
makeLenses ''Params

defaultParams :: Params
defaultParams = Params
  { _aliveStartChance = 0.6
  , _birthLimit = 3
  , _deathLimit = 4
  , _steps = 4
  }

parseParams :: Opt.Parser Params
parseParams = Params
  <$> Opt.option parseChance
      ( Opt.long "alive-start-chance"
      <> Opt.value (defaultParams ^. aliveStartChance)
      <> Opt.showDefault
      <> Opt.help ( "Chance for each cell to start alive at the beginning of "
                 <> "the cellular automata"
                 )
      <> Opt.metavar "CHANCE"
      )
  <*> Opt.option parseNeighbors
      ( Opt.long "birth-limit"
      <> Opt.value (defaultParams ^. birthLimit)
      <> Opt.showDefault
      <> Opt.help "Minimum neighbor count required for birth of a cell"
      <> Opt.metavar "NEIGHBORS"
      )
  <*> Opt.option parseNeighbors
      ( Opt.long "death-limit"
      <> Opt.value (defaultParams ^. deathLimit)
      <> Opt.showDefault
      <> Opt.help "Maximum neighbor count required for death of a cell"
      <> Opt.metavar "NEIGHBORS"
      )
  <*> Opt.option Opt.auto
      ( Opt.long "steps"
      <> Opt.value (defaultParams ^. steps)
      <> Opt.showDefault
      <> Opt.help "Number of generations to run the automata for"
      <> Opt.metavar "STEPS"
      )
  <**> Opt.helper
  where
    parseChance = readWithGuard
      (between 0 1)
      $ \res -> "Chance must be in the range [0,1], got: " <> show res

    parseNeighbors = readWithGuard
      (between 0 8)
      $ \res -> "Neighbors must be in the range [0,8], got: " <> show res

generate :: RandomGen g => Params -> Dimensions -> g -> Cells
generate params dims gen
  = runSTUArray
  $ fmap fst
  $ flip runRandT gen
  $ generate' params dims

generate' :: RandomGen g => Params -> Dimensions -> CellM g s (MCells s)
generate' params dims = do
  cells <- randInitialize dims $ params ^. aliveStartChance
  let steps' = params ^. steps
  when (steps' > 0)
   $ for_ [0 .. pred steps'] . const $ stepAutomata cells dims params
  -- Remove all but the largest contiguous region of unfilled space
  (_: smallerRegions) <- lift $ regions @UArray . amap not <$> freeze cells
  lift $ fillAllM (fold smallerRegions) cells
  lift $ fillOuterEdgesM cells
  pure cells

stepAutomata :: forall s g. MCells s -> Dimensions -> Params -> CellM g s ()
stepAutomata cells dims params = do
  origCells <- lift $ cloneMArray @_ @(STUArray s) cells
  for_ (range (0, V2 (dims ^. width) (dims ^. height))) $ \pos -> do
    neighs <- lift $ numAliveNeighborsM origCells pos
    origValue <- lift $ readArray origCells pos
    lift . writeArray cells pos
      $ if origValue
        then neighs >= params ^. deathLimit
        else neighs > params ^. birthLimit
