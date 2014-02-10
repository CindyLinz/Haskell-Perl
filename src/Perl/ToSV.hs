{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, ExistentialQuantification #-}
module Perl.ToSV
  where

import Control.Monad.IO.Class
import Foreign.C.Types
import Foreign.C.String

import Perl.Type
import Perl.Monad
import Perl.MonadGlue
import Perl.AsSV

(.=) :: (MonadIO m, ToSV a) => PtrSV -> a -> PerlT s m PtrSV
sv .= a = do
  setSV sv a
  return sv

class ToSV a where
  toSV :: MonadIO m => a -> PerlT s m PtrSV
  toSVMortal :: MonadIO m => a -> PerlT s m PtrSV
  setSV :: MonadIO m => PtrSV -> a -> PerlT s m ()

instance ToSV ToSVObj where
  toSV (ToSVObj a) = toSV a
  toSVMortal (ToSVObj a) = toSVMortal a
  setSV sv (ToSVObj a) = setSV sv a

instance ToSV PtrSV where
  toSV sv = do
    incRefCnt sv
    return sv
  toSVMortal = newSVSVMortal
  setSV = setSVSV

instance ToSV Int where
  toSV n = newIntSV (fromIntegral n)
  toSVMortal n = newIntSVMortal (fromIntegral n)
  setSV sv = setSVInt sv . fromIntegral

instance ToSV Double where
  toSV d = newNumSV (CDouble d)
  toSVMortal d = newNumSVMortal (CDouble d)
  setSV sv = setSVNum sv . CDouble

instance ToSV String where
  toSV str = PerlT $ \perl frames ->
    liftIO . withCStringLen str $ \(cstr, len) ->
      unPerlT (newStrSV cstr (fromIntegral len)) perl frames
  toSVMortal str = PerlT $ \perl frames ->
    liftIO . withCStringLen str $ \(cstr, len) ->
      unPerlT (newStrSVMortal cstr (fromIntegral len)) perl frames
  setSV sv str = PerlT $ \perl frames ->
    liftIO . withCStringLen str $ \(cstr, len) ->
      unPerlT (setSVStr sv cstr (fromIntegral len)) perl frames

asToSV :: (AsSV a, MonadIO m) => a -> PerlT s m PtrSV
asToSV a = asSV a >>= toSV

asToSVMortal :: (AsSV a, MonadIO m) => a -> PerlT s m PtrSV
asToSVMortal a = asSV a >>= toSVMortal

asSetSV :: (AsSV a, MonadIO m) => PtrSV -> a -> PerlT s m ()
asSetSV sv a = asSV a >>= setSV sv

instance ToSV RefSV where
  toSV = asToSV
  toSVMortal = asToSVMortal
  setSV = asSetSV
instance ToSV RefAV where
  toSV = asToSV
  toSVMortal = asToSVMortal
  setSV = asSetSV
instance ToSV RefHV where
  toSV = asToSV
  toSVMortal = asToSVMortal
  setSV = asSetSV
instance ToSV RefCV where
  toSV = asToSV
  toSVMortal = asToSVMortal
  setSV = asSetSV

class ToSVs a where
  toSVs :: MonadIO m => a -> PerlT s m [PtrSV]

instance ToSVs [PtrSV] where toSVs = mapM toSV
instance ToSVs [Int] where toSVs = mapM toSV
instance ToSVs [Double] where toSVs = mapM toSV
instance ToSVs [String] where toSVs = mapM toSV

data ToSVObj = forall a. ToSV a => ToSVObj a
