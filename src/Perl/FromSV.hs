{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
module Perl.FromSV
  where

import Control.Monad.IO.Class
import Foreign.C.Types
import Foreign.C.String

import Perl.Type
import Perl.Monad
import Perl.MonadGlue

class FromSV a where
  fromSVNon :: MonadIO m => PerlT s m a
  fromSV :: MonadIO m => SV -> PerlT s m a

instance FromSV SV where
  fromSVNon = newSV
  fromSV sv = do
    incRefCnt sv
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

class FromSVs a where
  fromSVs :: MonadIO m => [SV] -> PerlT s m a

instance FromSVs [SV] where fromSVs = mapM fromSV
instance FromSVs [Int] where fromSVs = mapM fromSV
instance FromSVs [Double] where fromSVs = mapM fromSV
instance FromSVs [String] where fromSVs = mapM fromSV
