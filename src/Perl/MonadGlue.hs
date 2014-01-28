module Perl.MonadGlue
  where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc

import Control.Monad
import Control.Monad.IO.Class

import Data.Array.Storable hiding (unsafeForeignPtrToStorableArray)
import Data.Array.Unsafe
import Data.Ix

import Perl.Type
import Perl.Glue
import Perl.Monad

------
--

newSV :: MonadIO m => StrLen -> PerlT s m PtrSV
newSV len = PerlT $ \perl frames -> do
  sv <- liftIO $ perl_newSV perl len
  return (frames, sv)

newStrSV :: MonadIO m => Ptr CChar -> StrLen -> CUInt -> PerlT s m PtrSV
newStrSV str len flag = PerlT $ \perl frames -> do
  sv <- liftIO $ perl_newSVpvn_flags perl str len flag
  return (frames, sv)

------
-- read SV

svToInt :: MonadIO m => PtrSV -> PerlT s m IV
svToInt sv = PerlT $ \perl frames -> do
  a <- liftIO $ svIVx perl sv
  return (frames, a)

svToNum :: MonadIO m => PtrSV -> PerlT s m NV
svToNum sv = PerlT $ \perl frames -> do
  a <- liftIO $ svNVx perl sv
  return (frames, a)

svToStr :: MonadIO m => PtrSV -> PerlT s m (ForeignPtr CChar, StrLen)
svToStr sv = PerlT $ \perl frames -> liftIO $ alloca $ \ptrLen -> do
  ptrStr <- svPVbytex perl sv ptrLen
  len <- peek ptrLen
  fptrStr <- newForeignPtr_ ptrStr
  return (frames, (fptrStr, len))

------
-- eval

eval :: MonadIO m => CString -> PerlT s m PtrSV
eval code = PerlT $ \perl frames -> do
  a <- liftIO $ perl_eval_pv perl code 1
  return (frames, a)

------
-- call

callVar :: MonadIO m => PtrSV -> CInt -> PerlT s m CInt
callVar sv flag = PerlT $ \perl frames -> do
  a <- liftIO $ perl_call_sv perl sv flag
  return (frames, a)

callName :: (Ix i, Ix j, Num j, MonadIO m) => CString -> CInt -> StorableArray i PtrSV -> PerlT s m (StorableArray j PtrSV)
callName name flag args = PerlT $ \perl frames -> liftIO . withStorableArray args $ \ptrArg -> alloca $ \ptrPtrOut -> do
  argc <- liftM (fromIntegral . rangeSize) (getBounds args)
  outn <- glue_call_pv perl name flag argc ptrArg ptrPtrOut
  if outn == 0
    then do
      fptrNull <- newForeignPtr_ nullPtr
      emptyArray <- unsafeForeignPtrToStorableArray fptrNull (fromIntegral 1, fromIntegral 0)
      return (frames, emptyArray)
    else do
      ptrOut <- peek ptrPtrOut
      fptrOut <- newForeignPtr p_free ptrOut
      outArray <- unsafeForeignPtrToStorableArray fptrOut (fromIntegral 1, fromIntegral outn)
      return (frames, outArray)
