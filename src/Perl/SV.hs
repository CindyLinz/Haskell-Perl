{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, ExistentialQuantification, Rank2Types #-}
module Perl.SV
  where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Data.Array.MArray

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Perl.Type
import Perl.Monad
import Perl.MonadGlue

-- | Copy out the value from a SV and then transform to the specified type
class FromSV a where
  fromSVNon -- ^ Used when the SV is not existed. This one should be the same as when the SV is undef
    :: MonadIO m => PerlT s m a
  fromSV :: MonadIO m => SV -> PerlT s m a

  -- ^ extract the LAST SV of an SVArray
  fromSVArray :: MonadIO m => SVArray -> PerlT s m a
  fromSVArray svArray = do
    lastIndex <- liftIO $ snd <$> getBounds svArray
    if lastIndex > 0
      then fromSV =<< liftIO (readArray svArray lastIndex)
      else fromSVNon

instance FromSV SV where
  fromSVNon = newSV
  fromSV = newSVSV

instance FromSV Int where
  fromSVNon = return 0
  fromSV sv = do
    int <- svToInt sv
    return (fromIntegral int)

instance FromSV Double where
  fromSVNon = return 0
  fromSV sv = do
    CDouble double <- svToNum sv
    return double

instance FromSV String where
  fromSVNon = return ""
  fromSV sv = do
    cStrLen <- svToStr sv
    liftIO $ peekCStringLen cStrLen

refFromSVNon :: MonadIO m => PerlT s m (Ptr a)
refFromSVNon = newSV >>= return . castPtr

refFromSV :: MonadIO m => SV -> PerlT s m (Ptr a)
refFromSV sv = newSVSV sv >>= return . castPtr

instance FromSV RefSV where
  fromSVNon = refFromSVNon
  fromSV = refFromSV
instance FromSV RefAV where
  fromSVNon = refFromSVNon
  fromSV = refFromSV
instance FromSV RefHV where
  fromSVNon = refFromSVNon
  fromSV = refFromSV
instance FromSV RefCV where
  fromSVNon = refFromSVNon
  fromSV = refFromSV

class FromSVs a where
  fromSVs :: MonadIO m => [SV] -> PerlT s m a

instance FromSVs [SV] where fromSVs = mapM fromSV
instance FromSVs [Int] where fromSVs = mapM fromSV
instance FromSVs [Double] where fromSVs = mapM fromSV
instance FromSVs [String] where fromSVs = mapM fromSV

-- | Something that could be transformed into an SV
class ToSV a where
  -- | Copy out the supplied value and transform into an SV
  toSV :: MonadIO m => a -> PerlT s m SV
  -- | Copy out the supplied value and transform into a mortal SV
  toSVMortal :: MonadIO m => a -> PerlT s m SV
  -- | Copy out the supplied value and transform set into an existed SV
  setSV :: MonadIO m => SV -> a -> PerlT s m ()

instance ToSV ToSVObj where
  toSV (ToSVObj a) = toSV a
  toSVMortal (ToSVObj a) = toSVMortal a
  setSV sv (ToSVObj a) = setSV sv a

instance ToSV SV where
  toSV = newSVSV
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

asToSV :: (AsSV a, MonadIO m) => a -> PerlT s m SV
asToSV a = asSV a >>= toSV

asToSVMortal :: (AsSV a, MonadIO m) => a -> PerlT s m SV
asToSVMortal a = asSV a >>= toSVMortal

asSetSV :: (AsSV a, MonadIO m) => SV -> a -> PerlT s m ()
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
  toSVs :: MonadIO m => a -> PerlT s m [SV]

instance ToSVs [SV] where toSVs = mapM toSV
instance ToSVs [Int] where toSVs = mapM toSV
instance ToSVs [Double] where toSVs = mapM toSV
instance ToSVs [String] where toSVs = mapM toSV

data ToSVObj = forall a. ToSV a => ToSVObj a

-- | Something that could be used as an SV directly
class AsSV a where
  -- | Use it as an SV, no copy (from the view of Perl)
  asSV :: MonadIO m => a -> PerlT s m SV

instance AsSV AsSVObj where
  asSV (AsSVObj a) = asSV a

instance AsSV SV where asSV = return
instance AsSV CV where asSV = return . castPtr
instance AsSV RefSV where asSV = return . castPtr
instance AsSV RefAV where asSV = return . castPtr
instance AsSV RefHV where asSV = return . castPtr
instance AsSV RefCV where asSV = return . castPtr

data AsSVObj = forall a. AsSV a => AsSVObj a
