module Main where

import Perl

main = do
  putStrLn "Init..."
  perl <- perlInit
  putStrLn "  done."

  putStrLn "Exit..."
  perlExit perl
  putStrLn "  done."
