module Main where

import Perl.Monad
import Perl.Eval
import Perl.FromSV
import Perl.ToSV
import Perl.Call
import Perl.Sub
import Perl.Type
import Perl.Ref
import Perl.AsSV
import Perl.AsRef
import Perl.AV
import Perl.HV
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Foreign.Ptr
import Data.Char

main = runPerlT $ do
  res <- eval $
    "sub call { my $func = shift; @res = $func->(@_); local $\" = ','; print qq( res: @res$/); @res }" ++
    "print 'Hi ', rand 10, $/;" ++
    "use Scalar::Util qw(dualvar);" ++
    "dualvar 3 + 4.5, 'Good'"
  n <- fromSV res
  d <- fromSV res
  s <- fromSV res
  liftIO . putStrLn $ show (n :: Int)
  liftIO . putStrLn $ show (d :: Double)
  liftIO $ putStrLn (s :: String)
  sinRetNum <- call "sin" (20 :: Int)
  liftIO $ putStrLn $ show (sinRetNum :: Double)

  cv <- sub $ \a b c extra -> {- subDo $ -} do
    let
      extraRet = " extra len = " ++ show (length (extra :: [SV]))
      det = b * b - 4 * a * c :: Double
    if det == 0
      then do
        liftIO $ putStrLn $ "1 ans = " ++ show (- b / (2 * a))
        retSub (-b / (2 * a), extraRet)
      else if det < 0
        then do
          liftIO $ putStrLn $ "0 ans"
          retSub ("No real roots", extraRet)
        else do
          liftIO $ putStrLn $ "2 ans"
          retSub ((-b + sqrt det) / (2 * a), (-b - sqrt det) / (2 * a), extraRet)
  noRet $ call "call" cv (1 :: Double) (2 :: Double) (1 :: Double) "a"
  noRet $ call "call" cv (1 :: Int) (1 :: Double) (1 :: Int)
  noRet $ call "call" cv (1 :: Int)
  res132 <- call "call" cv (1 :: Double) (3 :: Int) "2" "b" "c"
  liftIO $ putStrLn $ "res132 = " ++ show (res132 :: [String])

  defSub "my_test_add3" $ \a b c -> do
    liftIO $ putStrLn $ "a = " ++ show a ++ ", b = " ++ show b ++ ", c = " ++ show c
    retSub (a + b + c :: Double)

  my_test_add3_res <- call "my_test_add3" "1" (2 :: Int) (3 :: Double)
  liftIO $ putStrLn $ "my_test_add3 -> " ++ show (my_test_add3_res :: Int)

  noRet $ call "call" "my_test_add3" "3" (2 :: Int) (1 :: Double)

  defSub "cindy_is_beautiful" $ \n -> do
    liftIO $ putStrLn $ "CindyLinz is beautiful" ++ take n (repeat '!')
    retSub ()
  () <- call "cindy_is_beautiful" "5"

  defSub "makeSeq" $ \n -> do
    liftIO $ putStrLn $ show n
    av <- lift $ toAV [1..(n :: Int)]
    lift $ do
      when (n >= 1) $ writeAV av 0 "xx"
      pushAV av "last"
      unshiftAV av "first"
    avRef <- lift $ newRef av
    retSub avRef

  defSub "dumpSeq" $ \avRef -> do
    lift $ do
      av <- deRef avRef
      first <- shiftAV av
      last <- popAV av
      len <- lengthAV av
      liftIO $ putStrLn $ "first = " ++ first ++ ", last = " ++ last ++ ", len = " ++ show len
      forM_ (take (fromIntegral len) [0..]) $ \i -> do
        a <- readAV av i
        liftIO $ putStrLn $ "arr[" ++ show i ++ "] = " ++ show (a :: Int)

      intList <- fromAV av
      liftIO $ putStrLn $ "another dump " ++ show (intList :: [Int])

      e1 <- existsAV av 1
      clearAV av
      e2 <- existsAV av 1
      liftIO $ putStrLn $ "before clear: " ++ show e1 ++ ", after clear: " ++ show e2
    retSub ()

  eval "{ my $arr = makeSeq(3); use Data::Dumper; local $Data::Dumper::Indent = 0; print Dumper($arr),$/; dumpSeq(['first',3,4,5,'last']); }"

  defSub "defAscii" $ do
    hvRef <- lift $ do
      hv <- newHVEmpty
      forM_ ['A'..'G'] $ \c -> do
        writeHV hv [c] (ord c)
      newRef hv
    retSub hvRef

  defSub "invAscii" $ \hvRef keys -> do
    lift $ do
      hv <- deRef hvRef
      forM_ keys $ \k -> do
        n <- readHV hv (k :: String)
        writeHV hv k (-n :: Int)
    retSub ()

  defSub "deleteHash" $ \hvRef keys -> do
    lift $ do
      hv <- deRef hvRef
      forM_ keys $ \k -> do
        deleteHV_ hv (k :: String)
    retSub ()

  defSub "clearHash" $ \hvRef -> do
    lift $ deRef hvRef >>= clearHV
    retSub ()

  eval "{ my $ascii = defAscii(); local $Data::Dumper::Indent = 0; print Dumper($ascii),$/; invAscii($ascii, 'A', 'C', 'G'); print Dumper($ascii),$/; deleteHash($ascii, 'B', 'D', 'E'); print Dumper($ascii),$/; clearHash($ascii); print Dumper($ascii),$/; }"

  return ()
