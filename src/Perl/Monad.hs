{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, RankNTypes, TupleSections #-}
module Perl.Monad
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Catch

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc

import Perl.Type
import Perl.Class
import Perl.Internal.Glue

instance Exception PerlException where

releaseSVPool :: PtrPerl -> [SV] -> IO ()
releaseSVPool perl = mapM_ (svREFCNT_dec perl)

runPerlT :: (MonadCatch m, MonadIO m) => (forall s. PerlT s m a) -> m a
runPerlT act = do
  perl <- liftIO init_perl
  (svPool, a) <- unPerlT act perl nullPtr
  liftIO $ releaseSVPool perl svPool
  liftIO $ exit_perl perl
  return a

scope :: (MonadCatch m, MonadIO m) => PerlT s (PerlT s1 m) a -> PerlT s m a
scope (PerlT act) = PerlT $ \perl cv -> do
  flip (flip unPerlT perl) cv $ do
    (innerSV, a) <- act perl cv
    liftIO $ releaseSVPool perl innerSV
    return a

keepSV :: MonadIO m => SV -> PerlT s m ()
keepSV sv = PerlT $ \perl cv -> do
  liftIO $ svREFCNT_inc_NN sv
  return ([sv], ())
keepAV :: MonadIO m => AV -> PerlT s m ()
keepAV av = keepSV (castPtr av)
keepHV :: MonadIO m => HV -> PerlT s m ()
keepHV hv = keepSV (castPtr hv)

perlWithAnyIO :: MonadIO m => (forall b0. (a -> IO b0) -> IO b0) -> (a -> PerlT s IO b) -> PerlT s m b
perlWithAnyIO oriWith act =
  PerlT $ \perl cv -> do
    liftIO $ oriWith $ \a ->
      unPerlT (act a) perl cv

instance Functor m => Functor (PerlT s m) where
  fmap f k = PerlT $ \perl cv ->
    fmap (fmap f) (unPerlT k perl cv)

instance (MonadCatch m, MonadIO m, Monad m) => Monad (PerlT s m) where
  return a = PerlT $ \perl cv -> return ([], a)
  f >>= k = PerlT $ \perl cv -> do
    (frame1, a) <- unPerlT f perl cv
    onException
      ( do
        (frame2, b) <- unPerlT (k a) perl cv
        return (frame1 ++ frame2, b)
      )
      ( liftIO $ releaseSVPool perl frame1
      )

instance MonadTrans (PerlT s) where
  lift act = PerlT $ \_ _ -> act >>= return . pure

instance (MonadCatch m, MonadIO m) => MonadIO (PerlT s m) where
  liftIO = lift . liftIO

instance (MonadCatch m, MonadIO m) => MonadCatch (PerlT s m) where
  throwM = lift . throwM
  catch act handler = PerlT $ \perl cv -> catch (unPerlT act perl cv) (\e -> unPerlT (handler e) perl cv)
  mask act = PerlT $ \perl cv ->
    mask $ \unmask ->
      unPerlT (act $ \act2 -> PerlT $ \_ _ -> unmask $ unPerlT act2 perl cv) perl cv
  uninterruptibleMask act = PerlT $ \perl cv ->
    mask $ \unmask ->
      unPerlT (act $ \act2 -> PerlT $ \_ _ -> unmask $ unPerlT act2 perl cv) perl cv
