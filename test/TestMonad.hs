module Main where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Data.Char

import Foreign.Ptr

import Perl.Monad
import Perl.Eval
import Perl.Call
import Perl.Sub
import Perl.Type
import Perl.Ref
import Perl.SV
import Perl.AV
import Perl.HV
import Perl.Embed
import Perl.SVArray
--import Perl.Accessor

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
  noRet $ call "call" (cv, 1 :: Double, 2 :: Double, 1 :: Double, "a")
  noRet $ call "call" (cv, 1 :: Int, 1 :: Double, 1 :: Int)
  noRet $ call "call" (cv, 1 :: Int)
  res132 <- call "call" (cv, 1 :: Double, 3 :: Int, "2", "b", "c")
  liftIO $ putStrLn $ "res132 = " ++ show (res132 :: [String])

  defSub "my_test_add3" $ \a b c -> do
    context <- getSubContext
    liftIO $ putStrLn $ "my_test_add3 context=" ++ show context
    liftIO $ putStrLn $ "a = " ++ show a ++ ", b = " ++ show b ++ ", c = " ++ show c
    retSub (a + b + c :: Double)

  my_test_add3_res <- call "my_test_add3" ("1", 2 :: Int, 3 :: Double)
  liftIO $ putStrLn $ "my_test_add3 -> " ++ show (my_test_add3_res :: Int)

  noRet $ call "call" ("my_test_add3", "3", 2 :: Int, 1 :: Double)

  defSub "cindy_is_beautiful" $ \n -> do
    liftIO $ putStrLn $ "CindyLinz is beautiful" ++ take n (repeat '!')
    retSub ()
  () <- call "cindy_is_beautiful" "5"

  defSub "makeSeq" $ \n -> do
    liftIO $ putStrLn $ show n
    av <- toAV [1..(n :: Int)]
    when (n >= 1) $ writeAV av 0 "xx"
    pushAV av "last"
    unshiftAV av "first"
    avRef <- newRef av
    retSub avRef

  defSub "dumpSeq" $ \avRef -> do
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

  () <- eval "{ my $arr = makeSeq(3); use Data::Dumper; local $Data::Dumper::Indent = 0; print Dumper($arr),$/; dumpSeq(['first',3,4,5,'last']); }"

  defSub "defAscii" $ do
    hv <- newHVEmpty
    forM_ ['A'..'G'] $ \c -> do
      writeHV hv [c] (ord c)
    hvRef <- newRef hv
    retSub hvRef

  defSub "invAscii" $ \hvRef keys -> do
    hv <- deRef hvRef
    forM_ keys $ \k -> do
      n <- readHV hv (k :: String)
      writeHV hv k (-n :: Int)
    retSub ()

  defSub "deleteHash" $ \hvRef keys -> do
    hv <- deRef hvRef
    forM_ keys $ \k -> do
      deleteHV_ hv (k :: String)
    retSub ()

  defSub "clearHash" $ \hvRef -> do
    deRef hvRef >>= clearHV
    retSub ()

  defSub "echo" $ \str -> do
    liftIO $ putStrLn str
    retSub ()

  () <- eval "{ my $ascii = defAscii(); local $Data::Dumper::Indent = 0; print Dumper($ascii),$/; invAscii($ascii, 'A', 'C', 'G'); print Dumper($ascii),$/; deleteHash($ascii, 'B', 'D', 'E'); print Dumper($ascii),$/; clearHash($ascii); print Dumper($ascii),$/; }"
  () <- eval "{ echo 'CindyLinz is pretty' }"

  defSub "incA" $ do
    context <- getSubContext
    liftIO $ putStrLn $ "incA context=" ++ show context
    a <- readFindSV "$a"
    liftIO $ putStrLn $ "$a = " ++ show (a :: String)
    writeFindSV "$a"  (a ++ a)

    svB <- findSV "$b"
    b <- fromSV svB
    liftIO $ putStrLn $ "$b = " ++ show (b :: Int)
    setSV svB (b+1)
    retSub ()
  voidEval "sub f { my $a = 'oo'; my $b = 2; incA(); print ' then = ',$a,', ',$b,$/ }; f()";

  (defSub "anotherAdd" :: ToSVArray ret => Perl s ret -> Perl s ()) $ do
    context <- getSubContext
    liftIO $ putStrLn $ "anotherAdd context=" ++ show context
    args <- findAV "@_"
    liftIO $ putStrLn $ show args
    a <- readAV args 0
    b <- readAV args 1
    return (a + b :: Int)

  intListRes <- eval "sub add { anotherAdd() }; print '1 + 2 = ', add(1, 2), $/; (3, 5)"
  liftIO $ putStrLn $ show (intListRes :: [Int])

  () <- catch (eval "die 'fail'") (\errMsg -> liftIO $ putStrLn $ "dead: " ++ show (errMsg :: SomeException))

  defSub "willDie" $ do
    liftIO $ putStrLn $ "begin willDie"
    die "now dead"
    liftIO $ putStrLn $ "end willDie"
    retSub ()
  voidEval "eval { willDie() }; print '$@=', $@, $/"

--  defSub "accessor" $ do
--    v <- readScalar $ cap "%a" ~% "b" ~@ 1
--    liftIO $ putStrLn $ "accessor got: " ++ show (v :: Int)
--    writeScalar "nice" $ cap "%a" ~% "b" ~@ 2
--    avRef <- newRef =<< toAV ["a", "b", "c"]
--    writeScalar avRef $ cap "%a" ~% "b" ~@ 0
--    writeScalar "x" $ cap "@b" ~@ 1
--    retSub ()
--  defSub "accessor2" $ do
--    v <- readScalar $ cap "$_" ~% "x"
--    liftIO $ putStrLn $ "accessor2 got: " ++ v
--    retSub ()
--  voidEval $
--    "use Data::Dumper;" ++
--    "local $Data::Dumper::Indent = 0;" ++
--    "my %a = (a => 1, b => [1,2,3]);" ++
--    "my @b = qw(c b a);" ++
--    "accessor();" ++
--    "for({x=>1},{},{a=>2,x=>'o'}){ accessor2() }" ++
--    "print Dumper(\\%a),$/;" ++
--    "print Dumper(\\@b),$/;"
--
--  defSub "testMethod" $ do
--    obj <- callClass "A" "new" (3 :: Int) (4 :: Int) (5 :: Int)
--    liftIO $ putStrLn $ "obj = " ++ show obj
--    r <- callMethod obj "get" (2 :: Int)
--    liftIO $ putStrLn $ "r = " ++ show (r :: Int)
--    retSub ()
--  voidEval $
--    "{ package A; sub new { print qq(new @_$/); bless \\@_, $_[0] }; sub get { $_[0][$_[1]] } }" ++
--    "testMethod();"

  return ()
