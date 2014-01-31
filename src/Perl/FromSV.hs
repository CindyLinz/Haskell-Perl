{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
module Perl.FromSV
  where

import Control.Monad.IO.Class
import Foreign.C.Types
import Foreign.C.String

import Perl.Type
import Perl.Monad
import Perl.MonadGlue

class FromSV m a where
  fromSV :: PtrSV -> PerlT s m a

instance MonadIO m => FromSV m Int where
  fromSV sv = do
    int <- svToInt sv
    return (fromIntegral int)

instance MonadIO m => FromSV m Double where
  fromSV sv = do
    CDouble double <- svToNum sv
    return double

instance MonadIO m => FromSV m String where
  fromSV sv = do
    cStrLen <- svToStr sv
    liftIO $ peekCStringLen cStrLen
