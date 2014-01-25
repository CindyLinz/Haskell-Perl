{-# LANGUAGE Rank2Types #-}
module Perl.Monad
  where

import Foreign
import Data.Functor.Identity
import Control.Monad.IO.Class

import Perl.Glue

data SV = SV
data AV = AV
data HV = HV

data ScopeFrame = ScopeFrame
  { poolSV :: [Ptr SV]
  , poolAV :: [Ptr AV]
  , poolHV :: [Ptr HV]
  }
emptyFrame = ScopeFrame [] [] []

newtype PerlT s m a = PerlT { unPerlT :: [ScopeFrame] -> m ([ScopeFrame], a) }
type Perl s = PerlT s Identity

runPerlT :: MonadIO m => (forall s. PerlT s m a) -> m a
runPerlT act = do
  perl <- liftIO init_perl
  (_, a) <- unPerlT (scope act) []
  liftIO (exit_perl perl)
  return a

scope :: Monad m => (forall s. PerlT s m a) -> PerlT s m a
scope (PerlT act) = PerlT $ \scopeFrames -> do
  (headFrame : otherFrames, a) <- act (emptyFrame : scopeFrames)
  -- XXX clean headFrame
  return (otherFrames, a)

instance Monad m => Monad (PerlT s m) where
  return a = PerlT $ \scopeFrames -> return (scopeFrames, a)
  f >>= k = PerlT $ \scopeFrames -> do
    (scopeFrames', a) <- unPerlT f scopeFrames
    unPerlT (k a) scopeFrames'
