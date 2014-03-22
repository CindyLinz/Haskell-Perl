module Perl.Class
  where

import Control.Monad.Catch
import Control.Monad.IO.Class

import Perl.Type

class ToSVArray a where
  -- | Get a new SVArray with each element duplicated
  toSVArray :: (MonadCatch m, MonadIO m) => a -> PerlT s m SVArray
  -- | Get a new SVArray with each element duplicated and mortalized
  toSVMortalArray :: (MonadCatch m, MonadIO m) => a -> PerlT s m SVArray
  -- | Get a new SVArray and try to use the same element if possible
  asSVArray :: (MonadCatch m, MonadIO m) => a -> PerlT s m SVArray

