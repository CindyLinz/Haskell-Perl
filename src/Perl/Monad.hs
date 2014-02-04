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
