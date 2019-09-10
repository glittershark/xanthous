module Xanthous.Entities.Draw.Util where
--------------------------------------------------------------------------------
import Xanthous.Prelude
--------------------------------------------------------------------------------
import Brick.Widgets.Border.Style
import Brick.Types (Edges(..))
--------------------------------------------------------------------------------

borderFromEdges :: BorderStyle -> Edges Bool -> Char
borderFromEdges bstyle edges = ($ bstyle) $ case edges of
  Edges False False  False False -> const '☐'

  Edges True  False  False False -> bsVertical
  Edges False True   False False -> bsVertical
  Edges False False  True  False -> bsHorizontal
  Edges False False  False True  -> bsHorizontal

  Edges True  True   False False -> bsVertical
  Edges True  False  True  False -> bsCornerBR
  Edges True  False  False True  -> bsCornerBL

  Edges False True   True  False -> bsCornerTR
  Edges False True   False True  -> bsCornerTL
  Edges False False  True  True  -> bsHorizontal

  Edges False True   True  True  -> bsIntersectT
  Edges True  False  True  True  -> bsIntersectB
  Edges True  True   False True  -> bsIntersectL
  Edges True  True   True  False -> bsIntersectR

  Edges True  True   True  True  -> bsIntersectFull
