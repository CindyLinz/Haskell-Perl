{-# LANGUAGE RankNTypes #-}
-- |
-- Provide some functions when you need to define generic exception handling functions
module Perl.Monad.Exception
  ( perlMask
  , perlCatch
  ) where

import Perl.Monad

{- * Helper for Generic exception monad

A sample use is:

> instance Monad m => ExceptionMonad m where
>   gcatch = perlCatch gcatch
>   gmask = perlMask gmask

-}

perlMask :: Monad m => (forall a b. ((m a -> m a) -> m b) -> m b) -> ((PerlT s m a -> PerlT s m a) -> PerlT s m b) -> PerlT s m b
perlMask underMask outer = PerlT $ \perl frames -> underMask $ \restore -> do
  unPerlT (outer $ \inner -> PerlT $ \perl2 frames2 -> restore $ unPerlT inner perl2 frames2) perl frames

perlCatch :: Monad m => (forall a. m a -> (e -> m a) -> m a) -> PerlT s m a -> (e -> PerlT s m a) -> PerlT s m a
perlCatch underCatch comp handler = PerlT $ \perl frames -> underCatch (unPerlT comp perl frames) (\e -> unPerlT (handler e) perl frames)
