module Main where

import Perl.Monad
import Foreign.C.String
import Control.Monad.IO.Class

main = runPerlT $ do
  liftIO (newCString "print 'Hi ', rand 10, $/") >>= eval
