{-# LANGUAGE Rank2Types #-}
module Perl.Monad
  where

import Foreign
import Control.Monad
import Data.Functor.Identity
import Control.Monad.IO.Class

import Perl.Glue

data ScopeFrame = ScopeFrame
  { poolSV :: [PtrSV]
  , poolAV :: [PtrAV]
  , poolHV :: [PtrHV]
  }
emptyFrame = ScopeFrame [] [] []

newtype PerlT s m a = PerlT
  { unPerlT :: PtrPerl -> [ScopeFrame] -> m ([ScopeFrame], a)
  }
type Perl s = PerlT s Identity

runPerlT :: MonadIO m => (forall s. PerlT s m a) -> m a
runPerlT act = do
  perl <- liftIO init_perl
  (_, a) <- unPerlT (scope act) perl []
  liftIO (exit_perl perl)
  return a

scope :: MonadIO m => (forall s. PerlT s m a) -> PerlT s m a
scope (PerlT act) = PerlT $ \perl scopeFrames -> do
  let
    releasePool :: [Ptr a] -> IO ()
    releasePool ps = forM_ ps $ \p -> s_SvREFCNT_dec_NN perl (castPtr p)
  (ScopeFrame poolSV poolAV poolHV : otherFrames, a) <- act perl (emptyFrame : scopeFrames)
  liftIO $ do
    releasePool poolSV
    releasePool poolAV
    releasePool poolHV
  return (otherFrames, a)

instance Monad m => Monad (PerlT s m) where
  return a = PerlT $ \perl scopeFrames -> return (scopeFrames, a)
  f >>= k = PerlT $ \perl scopeFrames -> do
    (scopeFrames', a) <- unPerlT f perl scopeFrames
    unPerlT (k a) perl scopeFrames'

