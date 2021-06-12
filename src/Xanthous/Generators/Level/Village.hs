--------------------------------------------------------------------------------
module Xanthous.Generators.Level.Village
  ( fromCave
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (any, failing, toList)
--------------------------------------------------------------------------------
import           Control.Monad.Random (MonadRandom)
import           Control.Monad.State (execStateT, MonadState, modify)
import           Control.Monad.Trans.Maybe
import           Control.Parallel.Strategies
import           Data.Array.IArray
import           Data.Foldable (any, toList)
--------------------------------------------------------------------------------
import           Xanthous.Data
import           Xanthous.Data.EntityMap (EntityMap)
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Entities.Environment
import           Xanthous.Generators.Level.Util
import           Xanthous.Game.State (SomeEntity(..))
import           Xanthous.Random
--------------------------------------------------------------------------------

fromCave :: MonadRandom m
         => Cells -- ^ The positions of all the walls
         -> m (EntityMap SomeEntity)
fromCave wallPositions = execStateT (fromCave' wallPositions) mempty

fromCave' :: forall m. (MonadRandom m, MonadState (EntityMap SomeEntity) m)
          => Cells
          -> m ()
fromCave' wallPositions = failing (pure ()) $ do
  Just villageRegion <-
    choose
    . (`using` parTraversable rdeepseq)
    . weightedBy (\reg -> let circSize = length $ circumference reg
                         in if circSize == 50
                            then (1.0 :: Double)
                            else 1.0 / (fromIntegral . abs $ circSize - 50))
    $ regions closedHallways

  let circ = setFromList . circumference $ villageRegion

  centerPoints <- chooseSubset (0.1 :: Double) $ toList circ

  roomTiles <- foldM
              (flip $ const $ stepOut circ)
              (map pure centerPoints)
              [0 :: Int ..2]

  let roomWalls = circumference . setFromList @(Set _) <$> roomTiles
      allWalls = join roomWalls

  doorPositions <- fmap join . for roomWalls $ \room ->
    let candidates = filter (`notMember` circ) room
    in fmap toList . choose $ ChooseElement candidates

  let entryways =
        filter (\pt ->
                  let ncs = neighborCells pt
                  in any ((&&) <$> (not . (wallPositions !))
                              <*> (`notMember` villageRegion)) ncs
                   && any ((&&) <$> (`member` villageRegion)
                              <*> (`notElem` allWalls)) ncs)
                  $ toList villageRegion

  Just entryway <- choose $ ChooseElement entryways

  for_ (filter ((&&) <$> (`notElem` doorPositions) <*> (/= entryway)) allWalls)
    $ insertEntity Wall
  for_ (filter (/= entryway) doorPositions) $ insertEntity unlockedDoor
  insertEntity unlockedDoor entryway


  where
    insertEntity e pt = modify $ EntityMap.insertAt (ptToPos pt) $ SomeEntity e
    ptToPos pt = _Position # (fromIntegral <$> pt)

    stepOut :: Set (V2 Word) -> [[V2 Word]] -> MaybeT m [[V2 Word]]
    stepOut circ rooms = for rooms $ \room ->
      let nextLevels = hashNub $ toList . neighborCells =<< room
      in pure
         . (<> room)
         $ filter ((&&) <$> (`notMember` circ) <*> (`notElem` join rooms))
         nextLevels

    circumference pts =
      filter (any (`notMember` pts) . neighborCells) $ toList pts
    closedHallways = closeHallways livePositions
    livePositions = amap not wallPositions

--------------------------------------------------------------------------------

closeHallways :: Cells -> Cells
closeHallways livePositions =
  livePositions // mapMaybe closeHallway (assocs livePositions)
  where
    closeHallway (_, False) = Nothing
    closeHallway (pos, _)
      | isHallway pos = Just (pos, False)
      | otherwise     = Nothing
    isHallway pos = any ((&&) <$> not . view left <*> not . view right)
      . rotations
      . fmap (fromMaybe False)
      $ arrayNeighbors livePositions pos

failing :: Monad m => m a -> MaybeT m a -> m a
failing result = (maybe result pure =<<) . runMaybeT

{-

import Xanthous.Generators.Village
import Xanthous.Generators
import Xanthous.Data
import System.Random
import qualified Data.Text
import qualified Xanthous.Generators.CaveAutomata as CA
let gi = GeneratorInput SCaveAutomata CA.defaultParams
wallPositions <- generateFromInput gi (Dimensions 80 50) <$> getStdGen
putStrLn . Data.Text.unpack $ showCells wallPositions

import Data.Array.IArray
let closedHallways = closeHallways . amap not $ wallPositions
putStrLn . Data.Text.unpack . showCells $ amap not closedHallways

-}
