module Main where

import Perl.Monad
import Perl.Eval
import Perl.FromSV
import Perl.ToSV
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Control.Monad.IO.Class

main = runPerlT $ do
  res <- eval "print 'Hi ', rand 10, $/; use Scalar::Util qw(dualvar); dualvar 3 + 4.5, 'Good' "
  n <- fromSV res
  d <- fromSV res
  s <- fromSV res
  liftIO . putStrLn $ show (n :: Int)
  liftIO . putStrLn $ show (d :: Double)
  liftIO $ putStrLn (s :: String)
