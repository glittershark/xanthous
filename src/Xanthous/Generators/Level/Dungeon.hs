{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Xanthous.Generators.Level.Dungeon
  ( Params(..)
  , defaultParams
  , parseParams
  , generate
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding ((:>))
--------------------------------------------------------------------------------
import           Control.Monad.Random
import           Data.Array.ST
import           Data.Array.IArray (amap)
import           Data.Stream.Infinite (Stream(..))
import qualified Data.Stream.Infinite as Stream
import qualified Data.Graph.Inductive.Graph as Graph
import           Data.Graph.Inductive.PatriciaTree
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromJust)
import           Linear.V2
import           Linear.Metric
import qualified Options.Applicative as Opt
--------------------------------------------------------------------------------
import           Xanthous.Random
import           Xanthous.Data hiding (x, y, _x, _y, edges)
import           Xanthous.Generators.Level.Util
import           Xanthous.Util.Graphics (delaunay, straightLine)
import           Xanthous.Util.Graph (mstSubGraph)
--------------------------------------------------------------------------------

data Params = Params
  { _numRoomsRange :: (Word, Word)
  , _roomDimensionRange :: (Word, Word)
  , _connectednessRatioRange :: (Double, Double)
  }
  deriving stock (Show, Eq, Ord, Generic)
makeLenses ''Params

defaultParams :: Params
defaultParams = Params
  { _numRoomsRange = (6, 8)
  , _roomDimensionRange = (3, 12)
  , _connectednessRatioRange = (0.1, 0.15)
  }

parseParams :: Opt.Parser Params
parseParams = Params
  <$> parseRange
        "num-rooms"
        "number of rooms to generate in the dungeon"
        "ROOMS"
        (defaultParams ^. numRoomsRange)
  <*> parseRange
        "room-size"
        "size in tiles of one of the sides of a room"
        "TILES"
        (defaultParams ^. roomDimensionRange)
  <*> parseRange
        "connectedness-ratio"
        ( "ratio of edges from the delaunay triangulation to re-add to the "
        <> "minimum-spanning-tree")
        "RATIO"
        (defaultParams ^. connectednessRatioRange)
  <**> Opt.helper
  where
    parseRange name desc metavar (defMin, defMax) =
      (,)
      <$> Opt.option Opt.auto
          ( Opt.long ("min-" <> name)
          <> Opt.value defMin
          <> Opt.showDefault
          <> Opt.help ("Minimum " <> desc)
          <> Opt.metavar metavar
          )
      <*> Opt.option Opt.auto
          ( Opt.long ("max-" <> name)
          <> Opt.value defMax
          <> Opt.showDefault
          <> Opt.help ("Maximum " <> desc)
          <> Opt.metavar metavar
          )

generate :: RandomGen g => Params -> Dimensions -> g -> Cells
generate params dims gen
  = amap not
  $ runSTUArray
  $ fmap fst
  $ flip runRandT gen
  $ generate' params dims

--------------------------------------------------------------------------------

generate' :: RandomGen g => Params -> Dimensions -> CellM g s (MCells s)
generate' params dims = do
  cells <- initializeEmpty dims
  rooms <- genRooms params dims
  for_ rooms $ fillRoom cells

  let fullRoomGraph = delaunayRoomGraph rooms
      mst = mstSubGraph fullRoomGraph
      mstEdges = Graph.edges mst
      nonMSTEdges = filter (\(n₁, n₂, _) -> (n₁, n₂) `notElem` mstEdges)
                    $ Graph.labEdges fullRoomGraph

  reintroEdgeCount <- floor . (* fromIntegral (length nonMSTEdges))
                     <$> getRandomR (params ^. connectednessRatioRange)
  let reintroEdges = take reintroEdgeCount nonMSTEdges
      corridorGraph = Graph.insEdges reintroEdges mst

  corridors <- traverse
              ( uncurry corridorBetween
              . over both (fromJust . Graph.lab corridorGraph)
              ) $ Graph.edges corridorGraph

  for_ (join corridors) $ \pt -> lift $ writeArray cells pt True

  pure cells

type Room = Box Word

genRooms :: MonadRandom m => Params -> Dimensions -> m [Room]
genRooms params dims = do
  numRooms <- fromIntegral <$> getRandomR (params ^. numRoomsRange)
  subRand . fmap (Stream.take numRooms . removeIntersecting []) . infinitely $ do
    roomWidth <- getRandomR $ params ^. roomDimensionRange
    roomHeight <- getRandomR $ params ^. roomDimensionRange
    xPos <- getRandomR (0, dims ^. width - roomWidth)
    yPos <- getRandomR (0, dims ^. height - roomHeight)
    pure Box
      { _topLeftCorner = V2 xPos yPos
      , _dimensions = V2 roomWidth roomHeight
      }
  where
    removeIntersecting seen (room :> rooms)
      | any (boxIntersects room) seen
      = removeIntersecting seen rooms
      | otherwise
      = room :> removeIntersecting (room : seen) rooms
    streamRepeat x = x :> streamRepeat x
    infinitely = sequence . streamRepeat

delaunayRoomGraph :: [Room] -> Gr Room Double
delaunayRoomGraph rooms =
  Graph.insEdges edges . Graph.insNodes nodes $ Graph.empty
  where
    edges = map (\((n₁, room₁), (n₂, room₂)) -> (n₁, n₂, roomDist room₁ room₂))
          . over (mapped . both) snd
          . delaunay @Double
          . NE.fromList
          . map (\p@(_, room) -> (boxCenter $ fromIntegral <$> room, p))
          $ nodes
    nodes = zip [0..] rooms
    roomDist = distance `on` (boxCenter . fmap fromIntegral)

fillRoom :: MCells s -> Room -> CellM g s ()
fillRoom cells room =
  let V2 posx posy = room ^. topLeftCorner
      V2 dimx dimy = room ^. dimensions
  in for_ [posx .. posx + dimx] $ \x ->
       for_ [posy .. posy + dimy] $ \y ->
         lift $ writeArray cells (V2 x y) True

corridorBetween :: MonadRandom m => Room -> Room -> m [V2 Word]
corridorBetween originRoom destinationRoom
  = straightLine <$> origin <*> destination
  where
    origin = choose . NE.fromList =<< originEdge
    destination = choose . NE.fromList =<< destinationEdge
    originEdge = pickEdge originRoom originCorner
    destinationEdge = pickEdge destinationRoom destinationCorner
    pickEdge room corner = choose . over both (boxEdge room) $ cornerEdges corner
    originCorner =
      case ( compare (originRoom ^. topLeftCorner . _x)
                     (destinationRoom ^. topLeftCorner . _x)
           , compare (originRoom ^. topLeftCorner . _y)
                     (destinationRoom ^. topLeftCorner . _y)
           ) of
        (LT, LT) -> BottomRight
        (LT, GT) -> TopRight
        (GT, LT) -> BottomLeft
        (GT, GT) -> TopLeft

        (EQ, LT) -> BottomLeft
        (EQ, GT) -> TopRight
        (GT, EQ) -> TopLeft
        (LT, EQ) -> BottomRight
        (EQ, EQ) -> TopLeft -- should never happen

    destinationCorner = opposite originCorner
