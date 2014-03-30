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

asRefCommon :: (MonadCatch m, MonadIO m, AsRef a) => SV -> PerlT s m a
asRefCommon sv = do
  eitherRv <- safeAsRef sv
  case eitherRv of
    Right rv -> return rv
    Left msg -> do
      errSV <- toSVMortal msg
      throwM $ PerlException msg errSV

safeTestRV :: (MonadCatch m, MonadIO m, Monad n) => String -> (CInt -> Bool) -> SV -> PerlT s m (n (Ptr a))
safeTestRV typeName pred sv = do
  t <- rvType sv
  return $ if pred t
    then return $ castPtr sv
    else fail $ "cast " ++ typeName ++ " failed (svTYPE=" ++ show t ++ ")" 

instance AsRef RefSV where
  safeAsRef = safeTestRV "scalar ref" (< const_SVt_PVAV)
  asRef = asRefCommon

instance AsRef RefAV where
  safeAsRef = safeTestRV "array ref" (== const_SVt_PVAV)
  asRef = asRefCommon

instance AsRef RefHV where
  safeAsRef = safeTestRV "hash ref" (== const_SVt_PVHV)
  asRef = asRefCommon

instance AsRef RefCV where
  safeAsRef = safeTestRV "code ref" (== const_SVt_PVCV)
  asRef = asRefCommon

--instance AsRef RefGV where
--  safeAsRef = safeTestRV (== const_SVt_PVGV)
