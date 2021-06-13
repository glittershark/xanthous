{ mkDerivation, aeson, array, async, base, bifunctors, brick
, checkers, classy-prelude, comonad, comonad-extras, constraints
, containers, criterion, data-default, data-interval, deepseq
, directory, fgl, fgl-arbitrary, file-embed, filepath
, generic-arbitrary, generic-lens, groups, hgeometry
, hgeometry-combinatorial, hpack, JuicyPixels, lens
, lens-properties, lib, lifted-async, linear, megaparsec, mmorph
, monad-control, MonadRandom, mtl, optparse-applicative, parallel
, parser-combinators, pointed, QuickCheck, quickcheck-instances
, quickcheck-text, random, random-extras, random-fu, random-source
, Rasterific, raw-strings-qq, reflection, semigroupoids, semigroups
, splitmix, stache, streams, tasty, tasty-hunit, tasty-quickcheck
, text, text-zipper, tomland, transformers, vector, vty, witherable
, yaml, zlib
}:
mkDerivation {
  pname = "xanthous";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array async base bifunctors brick checkers classy-prelude
    comonad comonad-extras constraints containers criterion
    data-default data-interval deepseq directory fgl fgl-arbitrary
    file-embed filepath generic-arbitrary generic-lens groups hgeometry
    hgeometry-combinatorial JuicyPixels lens lifted-async linear
    megaparsec mmorph monad-control MonadRandom mtl
    optparse-applicative parallel parser-combinators pointed QuickCheck
    quickcheck-instances quickcheck-text random random-extras random-fu
    random-source Rasterific raw-strings-qq reflection semigroupoids
    semigroups splitmix stache streams text text-zipper tomland
    transformers vector vty witherable yaml zlib
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson array async base bifunctors brick checkers classy-prelude
    comonad comonad-extras constraints containers criterion
    data-default data-interval deepseq directory fgl fgl-arbitrary
    file-embed filepath generic-arbitrary generic-lens groups hgeometry
    hgeometry-combinatorial JuicyPixels lens lifted-async linear
    megaparsec mmorph monad-control MonadRandom mtl
    optparse-applicative parallel parser-combinators pointed QuickCheck
    quickcheck-instances quickcheck-text random random-extras random-fu
    random-source Rasterific raw-strings-qq reflection semigroupoids
    semigroups splitmix stache streams text text-zipper tomland
    transformers vector vty witherable yaml zlib
  ];
  testHaskellDepends = [
    aeson array async base bifunctors brick checkers classy-prelude
    comonad comonad-extras constraints containers criterion
    data-default data-interval deepseq directory fgl fgl-arbitrary
    file-embed filepath generic-arbitrary generic-lens groups hgeometry
    hgeometry-combinatorial JuicyPixels lens lens-properties
    lifted-async linear megaparsec mmorph monad-control MonadRandom mtl
    optparse-applicative parallel parser-combinators pointed QuickCheck
    quickcheck-instances quickcheck-text random random-extras random-fu
    random-source Rasterific raw-strings-qq reflection semigroupoids
    semigroups splitmix stache streams tasty tasty-hunit
    tasty-quickcheck text text-zipper tomland transformers vector vty
    witherable yaml zlib
  ];
  benchmarkHaskellDepends = [
    aeson array async base bifunctors brick checkers classy-prelude
    comonad comonad-extras constraints containers criterion
    data-default data-interval deepseq directory fgl fgl-arbitrary
    file-embed filepath generic-arbitrary generic-lens groups hgeometry
    hgeometry-combinatorial JuicyPixels lens lifted-async linear
    megaparsec mmorph monad-control MonadRandom mtl
    optparse-applicative parallel parser-combinators pointed QuickCheck
    quickcheck-instances quickcheck-text random random-extras random-fu
    random-source Rasterific raw-strings-qq reflection semigroupoids
    semigroups splitmix stache streams text text-zipper tomland
    transformers vector vty witherable yaml zlib
  ];
  prePatch = "hpack";
  homepage = "https://github.com/glittershark/xanthous#readme";
  description = "A WIP TUI RPG";
  license = lib.licenses.gpl3Only;
}
