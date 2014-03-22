{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies #-}
module Perl.Ref
  where

import Control.Monad.Catch
import Control.Monad.IO.Class

import Foreign.Ptr
import Foreign.C.Types

import Perl.Type
import Perl.Constant
import Perl.Monad
import Perl.Internal.MonadGlue
import Perl.SV

avoidNull :: (MonadCatch m, MonadIO m) => PerlT s m (Ptr a) -> PerlT s m (Ptr a)
avoidNull svM = do
  sv <- svM
  if sv == nullPtr
    then do
      let msg = "deref to NULL"
      errSV <- toSVMortal msg
      throwM $ PerlException msg errSV
    else return sv

class Refable a b | a -> b, b -> a where
  newRef :: (MonadCatch m, MonadIO m) => a -> PerlT s m b
  deRef :: (MonadCatch m, MonadIO m) => b -> PerlT s m a

instance Refable SV RefSV where
  newRef = newSVRef
  deRef = avoidNull . deRefSV

instance Refable AV RefAV where
  newRef = newAVRef
  deRef = avoidNull . deRefAV

instance Refable HV RefHV where
  newRef = newHVRef
  deRef = avoidNull . deRefHV

class AsRef a where
  safeAsRef :: (MonadCatch m, MonadIO m, Monad n) => SV -> PerlT s m (n a)
  asRef :: (MonadCatch m, MonadIO m) => SV -> PerlT s m a

asRefCommon :: (MonadCatch m, MonadIO m, AsRef a) => String -> SV -> PerlT s m a
asRefCommon typeName sv = do
  maybeRv <- safeAsRef sv
  case maybeRv of
    Just rv -> return rv
    Nothing -> do
      let msg = "cast " ++ typeName ++ " failed"
      errSV <- toSVMortal msg
      throwM $ PerlException msg errSV

safeTestRV :: (MonadCatch m, MonadIO m, Monad n) => (CInt -> Bool) -> SV -> PerlT s m (n (Ptr a))
safeTestRV pred sv = do
  t <- rvType sv
  return $ if pred t
    then return $ castPtr sv
    else fail ""

instance AsRef RefSV where
  safeAsRef = safeTestRV (< const_SVt_PVAV)
  asRef = asRefCommon "scalar ref"

instance AsRef RefAV where
  safeAsRef = safeTestRV (== const_SVt_PVAV)
  asRef = asRefCommon "array ref"

instance AsRef RefHV where
  safeAsRef = safeTestRV (== const_SVt_PVHV)
  asRef = asRefCommon "hash ref"

instance AsRef RefCV where
  safeAsRef = safeTestRV (== const_SVt_PVCV)
  asRef = asRefCommon "code ref"

--instance AsRef RefGV where
--  safeAsRef = safeTestRV (== const_SVt_PVGV)
