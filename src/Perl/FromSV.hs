{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
module Perl.FromSV
  where

import Control.Monad
import Control.Monad.IO.Class
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Perl.Type
import Perl.Monad
import Perl.MonadGlue

class FromSV a where
  fromSVNon :: MonadIO m => PerlT s m a
  fromSV :: MonadIO m => SV -> PerlT s m a

instance FromSV SV where
  fromSVNon = newSV
  fromSV sv = do
    keepSV sv
    return sv

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
refFromSV sv = keepSV sv >> (return $ castPtr sv)

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
