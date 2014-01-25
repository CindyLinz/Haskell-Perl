module Main where

import Perl
import Foreign.C.String

main = do
  perl <- perlInit

  newCString "print 'Hi ', rand 10, $/" >>= eval perl
  newCString "my $a = 123456789; $a =~ s/(?<=.)(?=(?:...)+$)/,/g; print $a, $/" >>= eval perl
  newCString "use Socket" >>= eval perl

  perlExit perl
