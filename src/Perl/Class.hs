module Perl.Class
  where

import Control.Monad.Catch
import Control.Monad.IO.Class

import Perl.Type

class ToSVArray a where
  toSVArray :: (MonadCatch m, MonadIO m) => a -> PerlT s m SVArray
  toSVMortalArray :: (MonadCatch m, MonadIO m) => a -> PerlT s m SVArray

