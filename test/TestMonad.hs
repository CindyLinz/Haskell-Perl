module Main where

import Perl.Monad
import Perl.Eval
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Control.Monad.IO.Class

main = runPerlT $ do
  eval "print 'Hi ', rand 10, $/"
