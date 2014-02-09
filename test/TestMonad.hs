module Main where

import Perl.Monad
import Perl.Eval
import Perl.FromSV
import Perl.ToSV
import Perl.Call
import Perl.Sub
import Perl.Type
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Control.Monad.IO.Class
import Data.Array.Storable

main = runPerlT $ do
  res <- eval "sub call { my $func = shift; @res = $func->(@_); local $\" = ','; print qq( res: @res$/) } print 'Hi ', rand 10, $/; use Scalar::Util qw(dualvar); dualvar 3 + 4.5, 'Good' "
  n <- fromSV res
  d <- fromSV res
  s <- fromSV res
  liftIO . putStrLn $ show (n :: Int)
  liftIO . putStrLn $ show (d :: Double)
  liftIO $ putStrLn (s :: String)
  sinRet <- call "sin" (20 :: Int)
  let
    sinRet_ = sinRet :: StorableArray Int PtrSV
  sinRet0 <- liftIO $ readArray sinRet 1
  sinRetNum <- fromSV sinRet0
  liftIO $ putStrLn $ show (sinRetNum :: Double)

  cv <- sub $ \a b c -> subDo $ do
    let det = b * b - 4 * a * c :: Double
    if det == 0
      then do
        liftIO $ putStrLn $ "1 ans = " ++ show (- b / (2 * a))
        return [ToSVObj (- b / (2 * a)) ]
      else if det < 0
        then do
          liftIO $ putStrLn $ "0 ans"
          return [ToSVObj "No real roots"]
        else do
          liftIO $ putStrLn $ "2 ans"
          return [ToSVObj ( (-b + sqrt det) / (2 * a) ), ToSVObj ( (-b - sqrt det) / (2 * a) )]
  noRet $ call "call" cv (1 :: Double) (2 :: Double) (1 :: Double)
  noRet $ call "call" cv (1 :: Double) (3 :: Int) "2"
  noRet $ call "call" cv (1 :: Int) (1 :: Double) (1 :: Int)
  return ()
