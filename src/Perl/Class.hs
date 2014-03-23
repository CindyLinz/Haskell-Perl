{-# LANGUAGE FlexibleContexts #-}
module Perl.Class
  where

import Data.Array.IArray

import Control.Monad.Catch
import Control.Monad.IO.Class

import Perl.Type

-- | Something that could be transformed into an SV
--   Minimal implementation: (toSV, toSVMortal, setSV) or asSV
class ToSV a where
  -- | Copy out the supplied value and transform into an SV
  toSV :: (ToSV SV, Monad (PerlT s m), MonadCatch m, MonadIO m) => a -> PerlT s m SV
  toSV a = asSV a >>= toSV
  -- | Copy out the supplied value and transform into a mortal SV
  toSVMortal :: (ToSV SV, Monad (PerlT s m), MonadCatch m, MonadIO m) => a -> PerlT s m SV
  toSVMortal a = asSV a >>= toSVMortal
  -- | Copy out the supplied value and transform set into an existed SV
  setSV :: (ToSV SV, Monad (PerlT s m), MonadCatch m, MonadIO m) => SV -> a -> PerlT s m ()
  setSV sv a = asSV a >>= setSV sv
  -- | Try to use it as an SV, no copy (from the view of Perl) if possible
  --   or use toSV instead
  asSV :: (ToSV SV, Monad (PerlT s m), MonadCatch m, MonadIO m) => a -> PerlT s m SV
  asSV = toSV

-- | Copy out the value from a SV and then transform to the specified type
class FromSV a where
  fromSVNon -- ^ Used when the SV is not existed. This one should be the same as when the SV is undef
    :: (MonadCatch m, MonadIO m) => PerlT s m a
  fromSV :: (MonadCatch m, MonadIO m) => SV -> PerlT s m a

class ToSVArray a where
  -- | Get a SVArray with each element duplicated
  toSVArray :: (MonadCatch m, MonadIO m) => a -> PerlT s m SVArray
  -- | Get a SVArray with each element duplicated and mortalized
  toSVMortalArray :: (MonadCatch m, MonadIO m) => a -> PerlT s m SVArray
  -- | Get a SVArray and try to use the same element if possible
  asSVArray :: (MonadCatch m, MonadIO m) => a -> PerlT s m SVArray

class FromSVArray a where
  fromSVArray :: (MonadCatch m, MonadIO m) => SVArray -> PerlT s m a

class ToSVList a where
  -- | Get a [SV] with each element duplicated
  toSVList :: (MonadCatch m, MonadIO m) => a -> PerlT s m [SV]
  -- | Get a [SV] with each element duplicated and mortalized
  toSVMortalList :: (MonadCatch m, MonadIO m) => a -> PerlT s m [SV]
  -- | Get a [SV] and try to use the same element if possible
  asSVList :: (MonadCatch m, MonadIO m) => a -> PerlT s m [SV]

class FromSVList a where
  fromSVList :: (MonadCatch m, MonadIO m) => [SV] -> PerlT s m a
