import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Verbosity
import Distribution.Simple.Program.Run
import Distribution.PackageDescription

main = defaultMainWithHooks simpleUserHooks
  { confHook = \pkg flag -> do
    perlOptc <- getProgramOutputList "perl" ["-MExtUtils::Embed", "-e", "ccopts"]
    perlOptl <- getProgramOutputList "perl" ["-MExtUtils::Embed", "-e", "ldopts"]

    originLocalBuildInfo <- confHook simpleUserHooks pkg flag
    let
      myBuildInfo = emptyBuildInfo
        { ccOptions = perlOptc
        , ldOptions = perlOptl
        }
    return originLocalBuildInfo
      { localPkgDescr = updatePackageDescription
        ( Just myBuildInfo
        , [ ("test-bare", myBuildInfo)
          , ("test-eval", myBuildInfo)
          ]
        )
        (localPkgDescr originLocalBuildInfo)
      }
  }
  where
    getProgramOutputList path args =
      fmap words $ getProgramInvocationOutput normal (simpleProgramInvocation path args)
