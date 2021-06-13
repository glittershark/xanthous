{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
--------------------------------------------------------------------------------
module Xanthous.Generators.Speech
  ( -- * Language definition
    Language(..)
    -- ** Lenses
  , phonotactics
  , syllablesPerWord

    -- ** Phonotactics
  , Phonotactics(..)
    -- *** Lenses
  , onsets
  , nuclei
  , codas
  , numOnsets
  , numNuclei
  , numCodas

    -- * Language generation
  , syllable
  , word

    -- * Languages
  , english
  , gormlak

  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude hiding (replicateM)
import           Data.Interval (Interval, (<=..<=))
import qualified Data.Interval as Interval
import           Control.Monad.Random.Class (MonadRandom)
import           Xanthous.Random (chooseRange, choose, ChooseElement (..), Weighted (Weighted))
import           Control.Monad (replicateM)
import           Test.QuickCheck (Arbitrary, CoArbitrary, Function)
import           Test.QuickCheck.Instances.Text ()
import           Data.List.NonEmpty (NonEmpty)
--------------------------------------------------------------------------------

newtype Phoneme = Phoneme Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, CoArbitrary, Function)
  deriving newtype (IsString, Semigroup, Monoid, Arbitrary)

-- | The phonotactics of a language
--
-- The phonotactics of a language represent the restriction on the phonemes in
-- the syllables of a language.
--
-- Syllables in a language consist of an onset, a nucleus, and a coda (the
-- nucleus and the coda together representing the "rhyme" of the syllable).
data Phonotactics = Phonotactics
  { _onsets    :: [Phoneme] -- ^ The permissible onsets, or consonant clusters
                           --   at the beginning of a syllable
  , _nuclei    :: [Phoneme] -- ^ The permissible nuclei, or vowel clusters in
                           --   the middle of a syllable
  , _codas     :: [Phoneme] -- ^ The permissible codas, or consonant clusters at
                           --   the end of a syllable
  , _numOnsets :: Interval Word -- ^ The range of number of allowable onsets
  , _numNuclei :: Interval Word -- ^ The range of number of allowable nuclei
  , _numCodas  :: Interval Word -- ^ The range of number of allowable codas
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
makeLenses ''Phonotactics

-- | Randomly generate a syllable with the given 'Phonotactics'
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

-- | A definition for a language
--
-- Currently this provides enough information to generate multi-syllabic words,
-- but in the future will likely also include grammar-related things.
data Language = Language
  { _phonotactics :: Phonotactics
  , _syllablesPerWord :: Weighted Int NonEmpty Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
makeLenses ''Language

word :: MonadRandom m => Language -> m Text
word lang = do
  numSyllables <- choose $ lang ^. syllablesPerWord
  mconcat <$> replicateM numSyllables (syllable $ lang ^. phonotactics)

--------------------------------------------------------------------------------

-- <https://en.wikipedia.org/wiki/English_phonology#Phonotactics>
englishPhonotactics :: Phonotactics
englishPhonotactics = Phonotactics
  { _onsets = [ "pl" , "bl" , "kl" , "gl" , "pr" , "br" , "tr" , "dr" , "kr"
              , "gr" , "tw" , "dw" , "gw" , "kw" , "pw"

              , "fl" , "sl" , {- "thl", -} "shl" {- , "vl" -}
              , "p", "b", "t", "d", "k", "ɡ", "m", "n", "f", "v", "th", "s"
              , "z", "h", "l", "w"

              , "sp", "st", "sk"

              , "sm", "sn"

              , "sf", "sth"

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
             , "mp", "nt", "nd", "nth", "nsh", "nk"
             , "mf", "ms", "mth", "nf", "nth", "ns", "nz", "nth"
             , "ft", "sp", "st", "sk"
             , "fth"
             , "pt", "kt"
             , "pth", "ps", "th", "ts", "dth", "dz", "ks"
             , "lpt", "lps", "lfth", "lts", "lst", "lkt", "lks"
             , "rmth", "rpt", "rps", "rts", "rst", "rkt"
             , "mpt", "mps", "ndth", "nkt", "nks", "nkth"
             , "ksth", "kst"
             ]
  , _numOnsets = 0 <=..<= 1
  , _numNuclei = Interval.singleton 1
  , _numCodas  = 0 <=..<= 1
  }

english :: Language
english = Language
  { _phonotactics = englishPhonotactics
  , _syllablesPerWord = Weighted [(20, 1),
                                  (7,  2),
                                  (2,  3),
                                  (1,  4)]
  }

gormlakPhonotactics :: Phonotactics
gormlakPhonotactics = Phonotactics
 { _onsets = [ "h", "l", "g", "b", "m", "n", "ng"
             , "gl", "bl", "fl"
             ]
 , _numOnsets = Interval.singleton 1
 , _nuclei = [ "a", "o", "aa", "u" ]
 , _numNuclei = Interval.singleton 1
 , _codas = [ "r", "l", "g", "m", "n"
            , "rl", "gl", "ml", "rm"
            , "n", "k"
            ]
 , _numCodas = Interval.singleton 1
 }

gormlak :: Language
gormlak = Language
  { _phonotactics = gormlakPhonotactics
  , _syllablesPerWord = Weighted [ (5, 2)
                                 , (5, 1)
                                 , (1, 3)
                                 ]
  }
