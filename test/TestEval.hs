module Main where

import Perl
import Foreign.C.String

main = do
  perlInit

  newCString "print 'Hi ', rand 10, $/" >>= eval
  newCString "my $a = 123456789; $a =~ s/(?<=.)(?=(?:...)+$)/,/g; print $a, $/" >>= eval

  perlExit
