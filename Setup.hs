import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Verbosity
import Distribution.Simple.Program.Run
import Distribution.PackageDescription

main = defaultMainWithHooks simpleUserHooks
  { preBuild = \_ _ -> do
    runProgramInvocation normal (simpleProgramInvocation "perl" ["-MExtUtils::Embed", "-e", "xsinit", "--", "-o", "c-src/perlxsi.c", "-std"])

    perlOptc <- getProgramOutputList "perl" ["-MExtUtils::Embed", "-e", "ccopts"]
    perlOptl <- getProgramOutputList "perl" ["-MExtUtils::Embed", "-e", "ldopts"]
    let
      myBuildInfo = emptyBuildInfo
        { ccOptions = perlOptc
        , ldOptions = perlOptl
        , cSources = ["c-src/perlxsi.c", "c-src/mini-padwalker.c"]
        }
    return
      ( Just myBuildInfo
      , [ ("test-bare", myBuildInfo)
        , ("test-eval", myBuildInfo)
        , ("test-monad", myBuildInfo)
        , ("test-monad-glue", myBuildInfo)
        ]
      )
  }
  where
    getProgramOutputList path args =
      fmap words $ getProgramInvocationOutput normal (simpleProgramInvocation path args)
