module Main where

import Perl.Monad
import Perl.MonadGlue
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Control.Monad
import Control.Monad.IO.Class
import Data.Array.Unsafe
import Data.Array.MArray

main = do
  putStrLn "test begin."
  perl
  putStrLn "test end."


perl = runPerlT $ do
  cmd1 <- liftIO $ newCString "sub call { my $func = shift; @res = $func->(@_); print qq( res: @res$/) } print 'Hi ', rand 10, ' ', sin(3), $/"
  eval cmd1
  liftIO $ free cmd1

  cmd2 <- liftIO $ newCString "use Scalar::Util qw(dualvar); dualvar 3.5, '^_^'"
  res2 <- eval cmd2
  liftIO $ free cmd2

  iv2 <- svToInt res2
  nv2 <- svToNum res2
  (pStr2, len2) <- svToStr res2
  liftIO $ do
    putStrLn $ "iv = " ++ show iv2
    putStrLn $ "nv = " ++ show nv2
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

  --subCV <- wrapSub (\perl cv -> putStrLn "Hello sub")
  subCV <- sub $ do
    args <- getSubArgs
    argList <- liftIO $ getElems args
    liftIO $ putStrLn "Hello sub:"
    forM_ argList $ \elem -> do
      i <- liftPerl $ svToInt elem
      liftIO $ putStrLn $ "  got: " ++ show i
      liftPerl $ setSVInt elem (i+i)
    liftIO (newListArray (1,2) $ reverse argList) >>= setSubReturns
  callStr <- liftIO $ newCString "call"
  callArgList <- forM [3,4,5] newNumSV
  callArgs <- liftIO $ newListArray (1,4) (castPtr subCV : callArgList)
  callName callStr 0 callArgs
  liftIO $ free callStr

  dieStr <- liftIO $ newCString "die"
  fptrNull <- liftIO $ newForeignPtr_ nullPtr
  emptyArgs <- liftIO $ unsafeForeignPtrToStorableArray fptrNull (1, 0)
  callName dieStr 8 emptyArgs
  liftIO $ free dieStr
