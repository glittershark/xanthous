{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Xanthous.Generators.Speech
  ( -- * Abstract phonotactics
    Phonotactics(..)
    -- ** Lenses
  , onsets
  , nuclei
  , codas
  , numOnsets
  , numNuclei
  , numCodas

    -- ** Definitions for languages
  , english

    -- * Language generation
  , syllable

  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (replicateM)
import           Data.Interval (Interval)
import qualified Data.Interval as Interval
import           Control.Monad.Random.Class (MonadRandom)
import           Xanthous.Random (chooseRange, choose, ChooseElement (ChooseElement))
import           Control.Monad (replicateM)
--------------------------------------------------------------------------------

newtype Phoneme = Phoneme Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
  deriving newtype (IsString, Semigroup, Monoid)

data Phonotactics = Phonotactics
  { _onsets    :: [Phoneme]
  , _nuclei    :: [Phoneme]
  , _codas     :: [Phoneme]
  , _numOnsets :: Interval Word
  , _numNuclei :: Interval Word
  , _numCodas  :: Interval Word
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
makeLenses ''Phonotactics

syllable :: MonadRandom m => Phonotactics -> m Text
syllable phonotactics = do
  let genPart num choices = do
        n <- fromIntegral . fromMaybe 0 <$> chooseRange (phonotactics ^. num)
        fmap (fromMaybe mempty . mconcat)
          . replicateM n
          . choose . ChooseElement
          $ phonotactics ^. choices

  (Phoneme onset) <- genPart numOnsets onsets
  (Phoneme nucleus) <- genPart numNuclei nuclei
  (Phoneme coda) <- genPart numCodas codas

  pure $ onset <> nucleus <> coda

--------------------------------------------------------------------------------

-- <https://en.wikipedia.org/wiki/English_phonology#Phonotactics>
english :: Phonotactics
english = Phonotactics
  { _onsets = [ "pl" , "bl" , "kl" , "gl" , "pr" , "br" , "tr" , "dr" , "kr"
              , "gr" , "tw" , "dw" , "gw" , "kw" , "pw"

              , "fl" , "sl" , {- "thl", -} "shl" {- , "vl" -}
              , "p", "b", "t", "d", "k", "ɡ", "m", "n", "f", "v", "th", "s"
              , "z", "h", "l", "w"

              , "sp", "st", "sk"

              , "sm", "sn"

              , "sf", "sθ"

              , "spl", "skl", "spr", "str", "skr", "skw", "sm", "sp", "st", "sk"
              ]
  , _nuclei = [ "a", "e", "i", "o", "u", "ur", "ar", "or", "ear", "are", "ure"
              , "oa", "ee", "oo", "ei", "ie", "oi", "ou"
              ]
  , _codas = [ "m", "n", "ng", "p", "t", "tsh", "k", "f", "sh", "s", "th", "x"
             , "v", "z", "zh", "l", "r", "w"

             , "lk", "lb", "lt", "ld", "ltsh", "ldsh", "lk"
             , "rp", "rb", "rt", "rd", "rtsh", "rdsh", "rk", "rɡ"
             , "lf", "lv", "lth", "ls", "lz", "lsh", "lth"
             , "rf", "rv", "rth", "rs", "rz", "rth"
             , "lm", "ln"
             , "rm", "rn", "rl"
             , "mp", "nt", "nd", "nth", "nsh", "ŋk"
             , "mf", "ms", "mth", "nf", "nth", "ns", "nz", "ŋθ"
             , "ft", "sp", "st", "sk"
             , "fth"
             , "pt", "kt"
             , "pth", "ps", "th", "ts", "dth", "dz", "ks"
             , "lpt", "lps", "lfth", "lts", "lst", "lkt", "lks"
             , "rmth", "rpt", "rps", "rts", "rst", "rkt"
             , "mpt", "mps", "ndth", "ŋkt", "ŋks", "ŋkth"
             , "ksth", "kst"
             ]
  , _numOnsets = Interval.singleton 1
  , _numNuclei = Interval.singleton 1
  , _numCodas  = Interval.singleton 1
  }
