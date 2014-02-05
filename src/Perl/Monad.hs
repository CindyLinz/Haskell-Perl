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

type ScopeFrame = [PtrSV]
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
    releasePool :: [PtrSV] -> IO ()
    releasePool ps = forM_ ps $ \p -> perl_sv_free perl p
  (poolSV : otherFrames, a) <- act perl (emptyFrame : scopeFrames)
  liftIO $ releasePool poolSV
  return (otherFrames, a)

keepSV :: MonadIO m => PtrSV -> PerlT s m ()
keepSV sv = PerlT $ \perl (pool : otherFrames) -> do
  liftIO $ svREFCNT_inc_NN sv
  return ((sv : pool) : otherFrames, ())
keepAV :: MonadIO m => PtrAV -> PerlT s m ()
keepAV av = keepSV (castPtr av)
keepHV :: MonadIO m => PtrHV -> PerlT s m ()
keepHV hv = keepSV (castPtr hv)

liftScope :: Monad m => (forall s. PerlT s m a) -> PerlT s m a
liftScope (PerlT act) = PerlT $ \perl (headFrame : otherFrames) -> do
  (otherFrames', a) <- act perl otherFrames
  return (headFrame : otherFrames', a)

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
  { unPerlSubT :: PtrPerl -> PtrCV -> m a
  }

liftPerl :: Monad m => PerlT s m a -> PerlSubT s m a
liftPerl act = PerlSubT $ \perl _ -> do
  ([], a) <- unPerlT act perl []
  return a

wrapSub :: MonadIO m => (PtrPerl -> PtrCV -> IO ()) -> PerlT s m PtrSV
wrapSub fun = PerlT $ \perl (frame:frames) -> do
  funPtr <- liftIO $ wrap_sub_wrapper fun
  cv <- liftIO $ wrap_sub perl funPtr
  return ((castPtr cv:frame):frames, cv)

sub :: MonadIO m => (forall m1. MonadIO m1 => PerlSubT s m1 ()) -> PerlT s m PtrSV
sub def = wrapSub $ \perl selfCV ->
  unPerlSubT def perl selfCV

instance Monad m => Monad (PerlSubT s m) where
  return a = PerlSubT $ \perl cv -> return a
  f >>= k = PerlSubT $ \perl cv -> do
    a <- unPerlSubT f perl cv
    unPerlSubT (k a) perl cv

instance MonadTrans (PerlSubT s) where
  lift act = PerlSubT $ \_ _ -> act

instance MonadIO m => MonadIO (PerlSubT s m) where
  liftIO = lift . liftIO
