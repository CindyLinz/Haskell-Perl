{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, ExistentialQuantification, Rank2Types #-}
module Perl.SV
  ( FromSV (..)
  , ToSV (..)
  , ToSVs (..)
  , globalSV
  , peekGlobalSV
  , ToSVObj (..)
  , isSV
  , isAV
  , isHV
  , isCV
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Array.MArray

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Perl.Type
import Perl.Constant
import Perl.Monad
import Perl.Internal.MonadGlue

peekGlobalSV :: (MonadCatch m, MonadIO m) => String -> PerlT s m (Maybe SV)
peekGlobalSV name = do
  sv <- perlWithAnyIO (withCStringLen name) (flip getSV 0)
  return $ if sv == nullPtr
    then Nothing
    else Just sv

globalSV :: (MonadCatch m, MonadIO m) => String -> PerlT s m SV
globalSV name = perlWithAnyIO (withCStringLen name) (flip getSV 1)

isSV, isAV, isHV, isCV :: (MonadCatch m, MonadIO m) => SV -> PerlT s m Bool
isSV sv = svType sv >>= return . (< const_SVt_PVAV)
isAV sv = svType sv >>= return . (== const_SVt_PVAV)
isHV sv = svType sv >>= return . (== const_SVt_PVHV)
isCV sv = svType sv >>= return . (== const_SVt_PVCV)

-- | Copy out the value from a SV and then transform to the specified type
class FromSV a where
  fromSVNon -- ^ Used when the SV is not existed. This one should be the same as when the SV is undef
    :: (MonadCatch m, MonadIO m) => PerlT s m a
  fromSV :: (MonadCatch m, MonadIO m) => SV -> PerlT s m a

  -- ^ extract the LAST SV of an SVArray
  fromSVArray :: (MonadCatch m, MonadIO m) => SVArray -> PerlT s m a
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

refFromSVNon :: (MonadCatch m, MonadIO m) => PerlT s m (Ptr a)
refFromSVNon = newSV >>= return . castPtr

refFromSV :: (MonadCatch m, MonadIO m) => SV -> PerlT s m (Ptr a)
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
  fromSVs :: (MonadCatch m, MonadIO m) => [SV] -> PerlT s m a

instance FromSVs [SV] where fromSVs = mapM fromSV
instance FromSVs [Int] where fromSVs = mapM fromSV
instance FromSVs [Double] where fromSVs = mapM fromSV
instance FromSVs [String] where fromSVs = mapM fromSV

-- | Something that could be transformed into an SV
--   Minimal implementation: (toSV, toSVMortal, setSV) or asSV
class ToSV a where
  -- | Copy out the supplied value and transform into an SV
  toSV :: (MonadCatch m, MonadIO m) => a -> PerlT s m SV
  toSV a = asSV a >>= toSV
  -- | Copy out the supplied value and transform into a mortal SV
  toSVMortal :: (MonadCatch m, MonadIO m) => a -> PerlT s m SV
  toSVMortal a = asSV a >>= toSVMortal
  -- | Copy out the supplied value and transform set into an existed SV
  setSV :: (MonadCatch m, MonadIO m) => SV -> a -> PerlT s m ()
  setSV sv a = asSV a >>= setSV sv
  -- | Try to use it as an SV, no copy (from the view of Perl) if possible
  --   or use toSV instead
  asSV :: (MonadCatch m, MonadIO m) => a -> PerlT s m SV
  asSV = toSV

instance ToSV ToSVObj where
  toSV (ToSVObj a) = toSV a
  toSVMortal (ToSVObj a) = toSVMortal a
  setSV sv (ToSVObj a) = setSV sv a
  asSV (ToSVObj a) = asSV a

instance ToSV SV where
  toSV = newSVSV
  toSVMortal = newSVSVMortal
  setSV = setSVSV
  asSV = return

instance ToSV () where
  toSV _ = newSV
  toSVMortal _ = newSVMortal
  setSV sv _ = setSVUndef sv

instance ToSV Int where
  toSV n = newIntSV (fromIntegral n)
  toSVMortal n = newIntSVMortal (fromIntegral n)
  setSV sv = setSVInt sv . fromIntegral

instance ToSV Double where
  toSV d = newNumSV (CDouble d)
  toSVMortal d = newNumSVMortal (CDouble d)
  setSV sv = setSVNum sv . CDouble

instance ToSV String where
  toSV str = PerlT $ \perl cv ->
    liftIO . withCStringLen str $ \(cstr, len) ->
      unPerlT (newStrSV cstr (fromIntegral len)) perl cv
  toSVMortal str = PerlT $ \perl cv ->
    liftIO . withCStringLen str $ \(cstr, len) ->
      unPerlT (newStrSVMortal cstr (fromIntegral len)) perl cv
  setSV sv str = PerlT $ \perl cv ->
    liftIO . withCStringLen str $ \(cstr, len) ->
      unPerlT (setSVStr sv cstr (fromIntegral len)) perl cv

class ToSVs a where
  toSVs :: (MonadCatch m, MonadIO m) => a -> PerlT s m [SV]

instance ToSVs [SV] where toSVs = mapM toSV
instance ToSVs [Int] where toSVs = mapM toSV
instance ToSVs [Double] where toSVs = mapM toSV
instance ToSVs [String] where toSVs = mapM toSV

data ToSVObj = forall a. ToSV a => ToSVObj a

instance ToSV RefSV where asSV = return . castPtr
instance ToSV RefAV where asSV = return . castPtr
instance ToSV RefHV where asSV = return . castPtr
instance ToSV RefCV where asSV = return . castPtr
