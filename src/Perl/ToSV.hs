{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, ExistentialQuantification #-}
module Perl.ToSV
  where

import Control.Monad.IO.Class
import Foreign.C.Types
import Foreign.C.String

import Perl.Type
import Perl.Monad
import Perl.MonadGlue

class ToSV m a where
  toSV :: a -> PerlT s m PtrSV

instance MonadIO m => ToSV m PtrSV where
  toSV sv = do
    incRefCnt sv
    return sv

instance MonadIO m => ToSV m Int where
  toSV n = newIntSV (fromIntegral n)

instance MonadIO m => ToSV m Double where
  toSV d = newNumSV (CDouble d)

instance MonadIO m => ToSV m String where
  toSV str = PerlT $ \perl frames ->
    liftIO . withCStringLen str $ \(cstr, len) ->
      unPerlT (newStrSV cstr (fromIntegral len) 0) perl frames

data ToSVObj m = forall a. ToSV m a => ToSVObj a
