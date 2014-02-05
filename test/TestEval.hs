module Main where

import Perl
import Perl.Glue
import Foreign.C.String

main = do
  perl <- perlInit
  perl2 <- perlInit

  newCString "print 'Hi ', rand 10, $/" >>= eval perl
  newCString "my $a = 123456789; $a =~ s/(?<=.)(?=(?:...)+$)/,/g; print $a, $/" >>= eval perl
  newCString "use Socket" >>= eval perl

  newCString "$a = 123" >>= eval perl
  newCString "$a = 321" >>= eval perl2
  newCString "print $a,$/" >>= eval perl
  newCString "print $a,$/" >>= eval perl2

  perlExit perl
  perlExit perl2
