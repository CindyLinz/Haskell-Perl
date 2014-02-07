{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, ExistentialQuantification #-}
module Perl.ToSV
  where

import Control.Monad.IO.Class
import Foreign.C.Types
import Foreign.C.String

import Perl.Type
import Perl.Monad
import Perl.MonadGlue

class ToSV a where
  toSV :: MonadIO m => a -> PerlT s m PtrSV

instance ToSV PtrSV where
  toSV sv = do
    incRefCnt sv
    return sv

instance ToSV Int where
  toSV n = newIntSV (fromIntegral n)

instance ToSV Double where
  toSV d = newNumSV (CDouble d)

instance ToSV String where
  toSV str = PerlT $ \perl frames ->
    liftIO . withCStringLen str $ \(cstr, len) ->
      unPerlT (newStrSV cstr (fromIntegral len) 0) perl frames

data ToSVObj = forall a. ToSV a => ToSVObj a
