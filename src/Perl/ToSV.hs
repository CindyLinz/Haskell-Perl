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
  toSVMortal :: MonadIO m => a -> PerlT s m PtrSV

instance ToSV PtrSV where
  toSV sv = do
    incRefCnt sv
    return sv
  toSVMortal = newSVSVMortal

instance ToSV Int where
  toSV n = newIntSV (fromIntegral n)
  toSVMortal n = newIntSVMortal (fromIntegral n)

instance ToSV Double where
  toSV d = newNumSV (CDouble d)
  toSVMortal d = newNumSVMortal (CDouble d)

instance ToSV String where
  toSV str = PerlT $ \perl frames ->
    liftIO . withCStringLen str $ \(cstr, len) ->
      unPerlT (newStrSV cstr (fromIntegral len)) perl frames
  toSVMortal str = PerlT $ \perl frames ->
    liftIO . withCStringLen str $ \(cstr, len) ->
      unPerlT (newStrSVMortal cstr (fromIntegral len)) perl frames

data ToSVObj = forall a. ToSV a => ToSVObj a
