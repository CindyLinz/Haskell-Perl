module Main where

import Perl.Monad
import Perl.MonadGlue
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Control.Monad.IO.Class
import Data.Array.Unsafe
import Data.Array.MArray

main = runPerlT $ do
  cmd1 <- liftIO $ newCString "print 'Hi ', rand 10, ' ', sin(3), $/"
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

  sinStr <- liftIO $ newCString "sin"
  threeStr <- liftIO $ newCString "3"
  threeSV <- newStrSV threeStr 1 0
  liftIO $ free threeStr
  sinArgs <- liftIO $ newArray (1,1) threeSV
  sinRet <- callName sinStr 0 sinArgs
  sinRet0 <- liftIO $ readArray sinRet 1
  sinRetNum <- svToNum sinRet0
  liftIO $ free sinStr
  liftIO $ putStrLn $ show sinRetNum

  dieStr <- liftIO $ newCString "die"
  fptrNull <- liftIO $ newForeignPtr_ nullPtr
  emptyArgs <- liftIO $ unsafeForeignPtrToStorableArray fptrNull (1, 0)
  callName dieStr 0 emptyArgs
  liftIO $ free dieStr
