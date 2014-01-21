module Main where

import Perl
import Foreign.C.String

main = do
  perlInit

  newCString "print 'Hi ', rand 10, $/" >>= eval

  perlExit
