--------------------------------------------------------------------------------
module Xanthous.App.Time
  ( stepGame
  , stepGameBy
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           System.Exit
--------------------------------------------------------------------------------
import           Xanthous.Data (Ticks)
import           Xanthous.App.Prompt
import qualified Xanthous.Data.EntityMap as EntityMap
import           Xanthous.Entities.Character (isDead)
import           Xanthous.Game.State
import           Xanthous.Game.Prompt
import           Xanthous.Game.Lenses
import           Control.Monad.State (modify)
import qualified Xanthous.Game.Memo as Memo
--------------------------------------------------------------------------------


stepGameBy :: Ticks -> AppM ()
stepGameBy ticks = do
  ents <- uses entities EntityMap.toEIDsAndPositioned
  for_ ents $ \(eid, pEntity) -> do
    pEntity' <- step ticks pEntity
    entities . ix eid .= pEntity'

  clearMemo Memo.characterVisiblePositions
  modify updateCharacterVision

  whenM (uses character isDead)
    . prompt_ @'Continue ["dead"] Uncancellable
    . const . lift . liftIO
    $ exitSuccess

ticksPerTurn :: Ticks
ticksPerTurn = 100

stepGame :: AppM ()
stepGame = stepGameBy ticksPerTurn
