{-# LANGUAGE TypeSynonymInstances, ExistentialQuantification, Rank2Types, FlexibleInstances #-}
module Perl.Accessor
  where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Foreign.Ptr
import Foreign.C.Types

import Perl.Type
import Perl.Monad
import Perl.Class
import Perl.SV
import Perl.AV
import Perl.HV
import Perl.Embed
import Perl.Ref
import Perl.Sub

{-
 my $data = { a => 3
 , b => [4, 5, 6]
 , c => { x => 1, y => 'abc', z => [3, 2, 1] }
 , d => [ sub { return $_[0] + $_[1] + 3 } ]
 , e => sub { return [3,$_[0]+1,5] }
 };


 v <- get $ "b" <.> 1
 -- v = 5
 set ("b" <.> 2) 7
 "b" <.> 2 .= 7
 -- b => [4, 5, 7]
 k <- call ("d" <.> 0) 2
 k <- "d" <.> 0 <$> 2 <*> 1
 -- k = 6

 sv <- cap "$data" ~% "b" -- sv = alias of av ref to [4,5,6]
 v <- get $ cap "$data" ~% "b" ~@ 1 -- v = 5
 v <- fromSV $ cap "$data" ~% "b" ~@ 1 -- v = 5
 cap "$data" ~% "b" ~@ 2 ~= 7 -- $data->{b} = [4,5,7]
 ? cap "$data" ~% "b" ~@ [2,3] ~= [4,5] -- $data->{b} = [4,5,4,5]
 k <- call (cap "$data" ~% "d" ~@ 0) 2 1 -- k = 6
 j <- fromSV $ call (cap "$data" ~% "e") 2 ~@ 1 -- j = 3
 ? cap "$data" ~% "c" ~% ("x", "z" ~@ 0) ~= (3, 5) -- $data->{c} = { x => 3, y => 'abc', z => [5, 2, 1] }

 -}

----cap :: String -> Accessor () SV
--
----(~%) :: Accessor a SV -> String -> Accessor a b
--
----type SVAccessor = Perl s SV
--
----Functor f. SV -> f (SV -> Perl s SV) -> Perl s (f SV)
--
--data SVExtractor a = SVExtractor { unSVExtractor :: forall s. SV -> Perl s a }
----(SVExtractor a, SVExtractor b) -> SVExtractor (a, b)
----[SVExtractor a] -> SVExtractor [a]
----Either (SVExtractor a) (SVExtractor b) -> SVExtractor (Either a b)
---- by Traversable
--
--instance Functor SVExtractor where
--  fmap f (SVExtractor ext) = SVExtractor $ \sv -> ext sv >>= return . f
--
--instance Monad SVExtractor where
--  return a = SVExtractor $ \_ -> return a
--  k >>= f = SVExtractor $ \sv -> do
--    a <- unSVExtractor k sv
--    unSVExtractor (f a) sv

class ScalarAccessor scalar where
  readScalar :: (FromSV a, MonadCatch m, MonadIO m) => PerlT s m scalar -> PerlT s m a
  writeScalar :: (ToSV a, MonadCatch m, MonadIO m) => a -> PerlT s m scalar -> PerlT s m ()
  writableScalar :: (MonadCatch m, MonadIO m) => scalar -> PerlT s m SV

instance ScalarAccessor SV where
  readScalar capSV = do
    sv <- capSV
    if sv == nullPtr
      then fromSVNon
      else fromSV sv
  writeScalar a capSV = do
    sv <- capSV
    if sv == nullPtr
      then return ()
      else setSV sv a
  writableScalar = return

data ArrayElement = ArrayElement AV {-# UNPACK #-} !CInt
instance ScalarAccessor ArrayElement where
  readScalar capAVE = do
    ArrayElement av i <- capAVE
    if av == nullPtr
      then fromSVNon
      else readAV av i
  writeScalar a capAVE = do
    ArrayElement av i <- capAVE
    if av == nullPtr
      then return ()
      else writeAV av i a
  writableScalar (ArrayElement av i) = do
    maybeSV <- fetchAV av i
    case maybeSV of
      Just sv -> return sv
      Nothing -> die "unwritable array"

data HashElement = HashElement HV String
instance ScalarAccessor HashElement where
  readScalar capHVE = do
    HashElement hv key <- capHVE
    if hv == nullPtr
      then fromSVNon
      else readHV hv key
  writeScalar a capHVE = do
    HashElement hv key <- capHVE
    if hv == nullPtr
      then return ()
      else writeHV hv key a
  writableScalar (HashElement av key) = do
    maybeSV <- fetchHV av key
    case maybeSV of
      Just sv -> return sv
      Nothing -> die "unwritable hash"

cap :: (MonadCatch m, MonadIO m) => String -> PerlT s m SV
cap name@(sigil:_) = case sigil of
  '$' -> findSV name
  '@' -> liftM castPtr $ findAV name
  '%' -> liftM castPtr $ findHV name

-- a ~@ i = access array or array ref
(~@) :: (ScalarAccessor sa, MonadCatch m, MonadIO m) => PerlT s m sa -> Int -> PerlT s m ArrayElement
capSA ~@ i = do
  avOrRef <- capSA >>= writableScalar
  av <- safeAsRef avOrRef >>= maybe (return $ castPtr avOrRef) deRef
  return $ ArrayElement av (fromIntegral i)

-- a ~% key = access hash or hash ref
(~%) :: (ScalarAccessor sa, MonadCatch m, MonadIO m) => PerlT s m sa -> String -> PerlT s m HashElement
capSA ~% key = do
  hvOrRef <- capSA >>= writableScalar
  hv <- safeAsRef hvOrRef >>= maybe (return $ castPtr hvOrRef) deRef
  return $ HashElement hv key

infixl 5 ~@, ~%

-- (a ~& func) arg1 arg2
(~&) :: (AsSV sv, MonadCatch m, MonadIO m) => sv -> String -> PerlT s m SV
(~&) = undefined

