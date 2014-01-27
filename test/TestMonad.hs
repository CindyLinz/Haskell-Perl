module Main where

import Perl.Monad
import Perl.MonadGlue
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Control.Monad.IO.Class

main = runPerlT $ do
  cmd1 <- liftIO $ newCString "print 'Hi ', rand 10, $/"
  eval cmd1
  liftIO $ free cmd1

  cmd2 <- liftIO $ newCString "use Scalar::Util qw(dualvar); dualvar 3.5, '^_^'"
  res2 <- eval cmd2
  liftIO $ free cmd2

  iv2 <- svToInt res2
  nv2 <- svToNum res2
  (fpStr2, len2) <- svToStr res2
  liftIO $ do
    putStrLn $ "iv = " ++ show iv2
    putStrLn $ "nv = " ++ show nv2
    withForeignPtr fpStr2 $ \pStr2 -> do
      str <- peekCStringLen (pStr2, fromIntegral len2)
      putStrLn $ "pv = " ++ str
