{-# LANGUAGE Rank2Types #-}
module Perl.Monad
  where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Perl.Type
import Perl.Glue

type ScopeFrame = [SV]
emptyFrame = []

newtype PerlT s m a = PerlT
  { unPerlT :: PtrPerl -> [ScopeFrame] -> m ([ScopeFrame], a)
  }
type Perl s = PerlT s IO

runPerlT :: MonadIO m => (forall s. PerlT s m a) -> m a
runPerlT act = do
  perl <- liftIO init_perl
  (_, a) <- unPerlT (scope act) perl []
  liftIO (exit_perl perl)
  return a

scope :: MonadIO m => (forall s. PerlT s m a) -> PerlT s m a
scope (PerlT act) = PerlT $ \perl scopeFrames -> do
  let
    releasePool :: [SV] -> IO ()
    releasePool ps = forM_ ps $ \p -> svREFCNT_dec perl p
  (poolSV : otherFrames, a) <- act perl (emptyFrame : scopeFrames)
  liftIO $ releasePool poolSV
  return (otherFrames, a)

keepSV :: MonadIO m => SV -> PerlT s m ()
keepSV sv = PerlT $ \perl (pool : otherFrames) -> do
  liftIO $ svREFCNT_inc_NN sv
  return ((sv : pool) : otherFrames, ())
keepAV :: MonadIO m => AV -> PerlT s m ()
keepAV av = keepSV (castPtr av)
keepHV :: MonadIO m => HV -> PerlT s m ()
keepHV hv = keepSV (castPtr hv)

liftScope :: Monad m => (forall s. PerlT s m a) -> PerlT s m a
liftScope (PerlT act) = PerlT $ \perl (headFrame : otherFrames) -> do
  (otherFrames', a) <- act perl otherFrames
  return (headFrame : otherFrames', a)

instance Functor m => Functor (PerlT s m) where
  fmap f k = PerlT $ \perl scopeFrames ->
    fmap (\(scopeFrames', a) -> (scopeFrames', f a)) (unPerlT k perl scopeFrames)

instance Monad m => Monad (PerlT s m) where
  return a = PerlT $ \perl scopeFrames -> return (scopeFrames, a)
  f >>= k = PerlT $ \perl scopeFrames -> do
    (scopeFrames', a) <- unPerlT f perl scopeFrames
    unPerlT (k a) perl scopeFrames'

instance MonadTrans (PerlT s) where
  lift act = PerlT $ \_ frames -> do
    a <- act
    return (frames, a)

instance MonadIO m => MonadIO (PerlT s m) where
  liftIO = lift . liftIO

newtype PerlSubT s m a = PerlSubT
  { unPerlSubT :: PtrPerl -> CV -> m a
  }
type PerlSub s = PerlSubT s (Perl s)

wrapSub :: MonadIO m => (PtrPerl -> CV -> IO ()) -> PerlT s m RefCV
wrapSub fun = PerlT $ \perl (frame:frames) -> liftIO $ do
  funPtr <- wrap_sub_wrapper fun
  cv <- wrap_sub perl funPtr
  return ((castPtr cv:frame):frames, cv)

makeSub :: MonadIO m => (forall s1. PerlSub s1 ()) -> PerlT s m RefCV
makeSub def = wrapSub $ \perl cv -> unPerlT (scope (unPerlSubT def perl cv)) perl [] >> return ()

regSub :: MonadIO m => CString -> (PtrPerl -> CV -> IO ()) -> PerlT s m ()
regSub name fun = PerlT $ \perl frames -> liftIO $ do
  funPtr <- wrap_sub_wrapper fun
  reg_sub perl name funPtr
  return (frames, ())

defineSub :: MonadIO m => CString -> (forall s1. PerlSub s1 ()) -> PerlT s m ()
defineSub name def = regSub name $ \perl cv -> unPerlT (scope (unPerlSubT def perl cv)) perl [] >> return ()

instance Functor m => Functor (PerlSubT s m) where
  fmap f k = PerlSubT $ \perl cv ->
    fmap f (unPerlSubT k perl cv)

instance Monad m => Monad (PerlSubT s m) where
  return a = PerlSubT $ \perl cv -> return a
  f >>= k = PerlSubT $ \perl cv -> do
    a <- unPerlSubT f perl cv
    unPerlSubT (k a) perl cv

instance MonadTrans (PerlSubT s) where
  lift act = PerlSubT $ \_ _ -> act

instance MonadIO m => MonadIO (PerlSubT s m) where
  liftIO = lift . liftIO
