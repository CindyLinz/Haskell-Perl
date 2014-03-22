module Main where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Data.Array.Unsafe
import Data.Array.IArray

import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr

import Perl.Constant
import Perl.Monad
import Perl.Internal.MonadGlue
import Perl.SVArray

main = do
  putStrLn "test begin."
  perl
  putStrLn "test end."


perl = runPerlT $ do
  cmd1 <- liftIO $ newCStringLen "sub call { my $func = shift; @res = $func->(@_); print qq( res: @res$/) } print 'Hi ', rand 10, ' ', sin(3), $/"
  eval cmd1 const_G_VOID
  liftIO $ free $ fst cmd1

  cmd2 <- liftIO $ newCStringLen "use Scalar::Util qw(dualvar); dualvar 3.5, '^_^'"
  res2arr <- eval cmd2 const_G_SCALAR
  let res2 = res2arr ! 1
  liftIO $ free $ fst cmd2

  iv2 <- svToInt res2
  nv2 <- svToNum res2
  (pStr2, len2) <- svToStr res2
  liftIO $ do
    putStrLn $ "iv = " ++ show iv2
    putStrLn $ "nv = " ++ show nv2
    str <- peekCStringLen (pStr2, fromIntegral len2)
    putStrLn $ "pv = " ++ str

  sinStr <- liftIO $ newCStringLen "sin"
  threeStr <- liftIO $ newCString "3"
  threeSV <- newStrSV threeStr 1
  liftIO $ free threeStr
  sinRet <- callName sinStr 0 (listArray (1,1) [threeSV])
  sinRetNum <- svToNum (sinRet ! 1)
  liftIO $ free $ fst sinStr
  liftIO $ putStrLn $ show sinRetNum

  --subCV <- wrapSub (\perl cv -> putStrLn "Hello sub")
  subCV <- makeSub $ do
    args <- getSubArgs
    let argList = elems args
    liftIO $ putStrLn "Hello sub:"
    forM_ argList $ \elem -> do
      i <- svToInt elem
      liftIO $ putStrLn $ "  got: " ++ show i
      setSVInt elem (i+i)
    return argList
  callStr <- liftIO $ newCStringLen "call"
  callArgList <- forM [3,4,5] newNumSV
  callName callStr 0 (listArray (1,4) (castPtr subCV : callArgList))
  liftIO $ free $ fst callStr

  dieStr <- liftIO $ newCStringLen "die"
  callName dieStr const_G_EVAL (array (1,0) [])
  liftIO $ free $ fst dieStr
