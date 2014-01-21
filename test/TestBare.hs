module Main where

import Perl

main = do
  putStrLn "Init..."
  perlInit
  putStrLn "  done."

  putStrLn "Exit..."
  perlExit
  putStrLn "  done."
