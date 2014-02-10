{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Perl.AsRef
  where

import Control.Monad.IO.Class
import Foreign.Ptr
import Foreign.C.Types

import Perl.Type
import Perl.Constant
import Perl.Monad
import Perl.MonadGlue

class AsRef a where
  safeAsRef :: (MonadIO m, Monad n) => SV -> PerlT s m (n a)
  asRef :: MonadIO m => SV -> PerlT s m a
  asRef sv = do
    maybeRv <- safeAsRef sv
    case maybeRv of
      Just rv -> return rv
      Nothing -> fail "asRv fail"

safeTestRV :: (MonadIO m, Monad n) => (CInt -> Bool) -> SV -> PerlT s m (n (Ptr a))
safeTestRV pred sv = do
  t <- rvType sv
  return $ if pred t
    then return $ castPtr sv
    else fail ""

instance AsRef RefSV where
  safeAsRef = safeTestRV (< const_SVt_PVAV)

instance AsRef RefAV where
  safeAsRef = safeTestRV (== const_SVt_PVAV)

instance AsRef RefHV where
  safeAsRef = safeTestRV (== const_SVt_PVHV)

instance AsRef RefCV where
  safeAsRef = safeTestRV (== const_SVt_PVCV)

--instance AsRef RefGV where
--  safeAsRef = safeTestRV (== const_SVt_PVGV)
