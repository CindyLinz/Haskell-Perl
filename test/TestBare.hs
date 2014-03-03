module Main where

import Perl.Internal.Glue

main = do
  putStrLn "Init..."
  perl <- init_perl
  putStrLn "  done."

  putStrLn "Exit..."
  exit_perl perl
  putStrLn "  done."
