module Main where

import Perl
import Perl.Glue

main = do
  putStrLn "Init..."
  perl <- perlInit
  putStrLn "  done."

  putStrLn "Exit..."
  perlExit perl
  putStrLn "  done."
